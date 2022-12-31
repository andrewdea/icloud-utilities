;;; icloud.el --- utilities to easily download files from iCloud  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Andrew De Angelis

;; Author: Andrew De Angelis <bobodeangelis@gmail.com>
;; Keywords: comm, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is still a sketch, but the main idea is to:
;; 1) provide a way to seamlessly check if a file is not present locally,
;; and if so, download it (`icloud-find-file')
;; 2) provide a way to seamlessly open the iCloud document rather than the
;; ".icloud" local copy (`icloud-navigation-mode')
;; 3) provide a way to download multiple files (`icloud-download-in-dir')
;; 4) provide utilities to do these things asynchronously when needed

;;; Code:
(defgroup icloud nil
  "Utilities for iCloud files."
  :group 'comm)

;; TODO: maybe there should also be a customizable multiplier that is tied to file-size
;; eg: every megabite, add a second
(defcustom icloud-max-wait 3
  "Integer denoting how many seconds `icloud-download' should wait.
If the file still isnt' downloaded after this amount of seconds,
error out."
  :type 'integer
  :group 'icloud)

(defface icloud-message
  '((t :inherit minibuffer-prompt))
  "Face used to display message about iCloud"
  :group 'icloud)

(defface icloud-error
  '((t :inherit error))
  "Face used to display error messages about iCloud"
  :group 'icloud)

(defun icloud-shell-command (file)
  "Build the shell command needed to download FILE."
  (concat
   "brctl download \"" file "\""))

(defun icloud-download (file)
  (shell-command (icloud-shell-command file))
  (message "%s %s"
           (propertize "Downloading:" 'face 'icloud-message)
           file))

(defun icloud-check-on-progress (file)
  (let ((start-time (current-time)))
    (while (and
            (not (file-exists-p file))
            (> icloud-max-wait (nth 1 (time-subtract nil start-time))))
      (sleep-for 0.005)))
  (file-exists-p file)) ; return nil when download doesn't succeed

(defun icloud-log (to-message &rest string)
  "Log STRING to a buffer."
  ;; (message "string: %s" string)
  ;; (message "(car string): %s" (car string))
  (let ((buf (get-buffer-create "*icloud-progress-log*"))
        (string (apply #'format string)))
    (when to-message
      (message string))
    (with-current-buffer buf
      (end-of-buffer)
      (insert string "\n"))))

(defun icloud-report-on-progress (file &optional error to-message)
  "Check on progress and log about it in *icloud-progrss-log*.
If ERROR is non-nil, create an error when the file isn't downloaded.
If TO-MESSAGE is non-nil, also send progress as `message'"
  (let ((success (icloud-check-on-progress file)))
    (if success
        (progn
          (icloud-log
           to-message
           "%s %s"
           (propertize "Download succeeded: " 'face 'icloud-message)
           file)
          file) ;; return the file name
      (let ((failed-message
             (format "%s %s"
                     (propertize "Failed to download this file:" 'face 'icloud-error)
                     file)))
        (icloud-log to-message failed-message)
        (when error
          (error failed-message))
        nil))))

(defun icloud-local-to-download (path)
  (let* ((path (expand-file-name path))
         (filename (file-name-nondirectory path))
         (length (length filename)))
    ;; (message "path: %s" path)
    ;; (message "filename: %s" filename)
    ;; (message "length: %s" length)
    (if (and (> length 7)
             (equal (substring filename -7 length) ".icloud")
             (eq (aref filename 0) ?.))
        (concat
         (file-name-directory path)
         (substring filename 1 -7))
      nil)))

;;;###autoload
(defun icloud-get-file (file &optional error)
  (if (file-exists-p file)
      file
    (progn
      (icloud-download file)
      (icloud-report-on-progress file error 'to-message))))

;;;###autoload
(defun icloud-get-file-if-cloud (file &optional error)
  (let ((cloud-file (icloud-local-to-download file)))
    (if cloud-file
        (icloud-get-file cloud-file error)
      file)))

(defun icloud-find-file (file)
  "This is intended for programmatic use: it expects a FILENAME
without trailing \".icloud\", so your program won't have to worry about
the FILENAME changing when the file is in the cloud.
For interactive use, see `icloud-get-file-if-cloud' and `icloud-navigation-mode'."
  (find-file (icloud-get-file file)))

(defun adv/icloud-download-if-cloud (filename)
  "Function used as advice to `insert-file-contents'.
It ensures that, if FILENAME is in the cloud, we download it before opening it.
This is done by calling `icloud-download-if-cloud' while filtering arguments
to `insert-file-contents'"
  (let ((downloaded-file
         (icloud-get-file-if-cloud (car filename) 'error)))
    ;; apparently the buffer-name is set before `insert-file-contents',
    ;; so we have to rename it with the non-".icloud" name
    ;; TODO: fix this; maybe the download should happen earlier?
    ;; need to look into when the buffer-name is created
    (rename-buffer (file-name-nondirectory downloaded-file) 'unique)
    (list downloaded-file (cdr filename))))

;;;###autoload
(define-minor-mode icloud-navigation-mode
  "Minor mode to download iCloud files when we try to open \".icloud\" files.
this uses `icloud-download-if-cloud', which converts any file with
trailing \".icloud\" into its expected format, and tries to download it."
  :global t
  :lighter " iCloud"
  :group 'comm
  (if icloud-navigation-mode
      (advice-add 'insert-file-contents :filter-args
                  #'adv/icloud-download-if-cloud)
    (advice-remove 'insert-file-contents #'adv/icloud-download-if-cloud)))

;; TODO: maybe `icloud-download-if-local' should be broken down into 2 parts:
;; in this case, all the checks are useless, since we know the arguments we send
;; fit the bill
;; TODO: there must a better approach to interactive/default arguments?
;;;###autoload
(defun icloud-download-in-directory (&optional directory regexp recursively report)
  "Find all iCloud files matching REGEXP in DIRECTORY, and download them.
If RECURSIVELY is non-nil, also find files within directories inside DIRECTORY.
If REPORT is non-nil, start an async thread to check on download progress.
To check they are in the cloud, REGEXP is altered to include a beginning
\"\\..*\" and a trailing \".*\\.icloud\""
  (interactive)
  (let* ((directory
          (or directory
              (when (called-interactively-p 'any)
                (read-directory-name
                 "Download iCloud files in this directory: "))
              default-directory))
         (regexp
          (or regexp
              (when (called-interactively-p 'any)
                (read-string
                 "Download files matching regexp (empty means all files): "))
              ""))
         (regexp (concat "\\..*" regexp ".*\\.icloud"))
         (recursively
          (or recursively
              (when (called-interactively-p 'any)
                (length
                 (read-string
                  "Recursively? (empty means NO, anything else is YES) ")))))
         (report
          (or report
              (when (called-interactively-p 'any)
                (length
                 (read-string
                  "Report on progress? (empty means NO, else YES) "))))))
    (icloud-log
     'to-message
     "%s %s,\n%s %s, \n%s %s, %s %s."
     (propertize "-----------\nDownloading in directory:" 'face 'icloud-message)
     directory
     (propertize "for regexp:" 'face 'icloud-message) regexp
     (propertize "recursively?" 'face 'icloud-message) (not (not recursively))
     (propertize "reporting?" 'face 'icloud-message) (not (not report)))
    (let ((files (mapcar #'icloud-local-to-download
                         (if recursively
                             (directory-files-recursively directory regexp)
                           (directory-files directory 'full regexp)))))
      (shell-command
       (mapconcat #'icloud-shell-command files " && "))
      (if report
          (make-thread
           (lambda ()
             (mapc #'icloud-report-on-progress files)
             (icloud-log 'to-message
                         (propertize
                          "iCloud report finished for all documents\n"
                          'face 'icloud-message))))))))

(provide 'icloud)
;;; icloud.el ends here
