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
;; and if so, download it (`icloud-get-file')
;; 2) provide a way to seamlessly open the iCloud document rather than the
;; ".icloud" local copy (`icloud-navigation-mode' and `icloud-open')
;; 3) provide a way to download multiple files (`icloud-download-files' and
;; `icloud-download-in-directory')

;;; Code:

;;;; core utilities
(defgroup icloud nil
  "Utilities for iCloud files."
  :group 'comm)

;; TODO: maybe there should also be a customizable multiplier that is tied to file-size
;; eg: every x megabites, add a second to the wait time
(defcustom icloud-max-wait 3
  "Integer denoting how many seconds `icloud-check-on-progress' should wait."
  :type 'integer
  :group 'icloud)

(defcustom icloud-default-open-function #'find-file
  "Function to open files within `icloud-open'."
  :type 'function
  :group 'icloud)

(defcustom icloud-destination-directory
  "~/Library/Mobile\  Documents/com~apple~CloudDocs/"
  "Default 'iCloud Drive' directory to move/copy files to."
  :type 'directory
  :group 'icloud)

(defface icloud-message
  '((t :inherit minibuffer-prompt))
  "Face used to display messages about iCloud."
  :group 'icloud)

(defface icloud-error
  '((t :inherit error))
  "Face used to display error-messages about iCloud."
  :group 'icloud)

(defun icloud-propertize-message (str)
  "Apply `propertize' to STR with the face `icloud-message'."
  (propertize str 'face 'icloud-message))

(defun icloud-shell-command (file)
  "Build the shell command needed to download FILE."
  (concat
   ;; TODO: maybe this should be a customizable variable
   ;; although it doesn't seem like there are many other options:
   ;; 1) 'brctl' is the command needed to download
   ;; 2) it doesn't take many parameters: just the one filename
   "brctl download \"" file "\""))

(defun icloud-start-download (file)
  "Start downloading FILE from iCloud, using `icloud-shell-command'."
  ;; TODO: it's possible that additional steps might be needed
  ;; such as `'brctl evict' (see https://apple.stackexchange.com/questions/328329/is-there-a-way-via-the-command-line-to-cause-icloud-files-to-download)
  ;; although this step is automatic in my system
  (shell-command (icloud-shell-command file))
  (message "%s: %s"
           (icloud-propertize-message "Downloading")
           file))
;; TODO: it might be good to use'brctl log' instead here
;; (see https://apple.stackexchange.com/questions/328329/is-there-a-way-via-the-command-line-to-cause-icloud-files-to-download)
;; although I'll have to get familiar with its output
(defun icloud-check-on-progress (file)
  "Check if FILE exists.  Wait for no more than `icloud-max-wait' seconds."
  (let ((start-time (current-time)))
    (while (and
            (not (file-exists-p file))
            ;; get the seconds passed since `start-time':
            (> icloud-max-wait (nth 1 (time-subtract nil start-time))))
      (sleep-for 0.005)))
  (file-exists-p file)) ; return nil when download doesn't succeed

(defun icloud-log (to-message &rest string)
  "Log STRING to a *icloud-progress-log* buffer.
If TO-MESSAGE is non-nil, also `message' STRING"
  (let ((buf (get-buffer-create "*icloud-progress-log*"))
        (string (apply #'format string)))
    (when to-message
      (message string))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string "\n"))))

(defun icloud-report-on-progress (file &optional error to-message)
  "Check on FILE's download progress and log it in *icloud-progress-log*.
Use `icloud-check-on-progress' to wait for the download to succeed.
If FILE doesn't exist after waiting for `icloud-max-wait', return nil.
If ERROR is non-nil, throw an error when the file isn't downloaded.
If TO-MESSAGE is non-nil, also use `message' to display progress"
  (let ((success (icloud-check-on-progress file)))
    (if success
        (progn
          (icloud-log
           to-message
           "%s %s"
           (icloud-propertize-message "Download succeeded:")
           file)
          file) ;; return the file name
      (let ((failed-message
             (format "%s %s"
                     (propertize "Failed to download this file:"
                                 'face 'icloud-error)
                     file)))
        (icloud-log to-message failed-message)
        (when error
          (error failed-message))
        nil))))

(defun icloud-local-to-download (path)
  "Convert PATH to its name in the cloud.
If PATH is not a cloud file, return nil.
If PATH is a non-downloaded local copy of an iCloud file
\(eg \"my_directory/.my_file.txt.icloud\"),
return its name stripped of the starting \".\" and the trailing \".icloud\"
\(eg \"my_directory/my-file.txt\")"
  (let* ((path (expand-file-name path))
         (filename (file-name-nondirectory path))
         (length (length filename)))
    (if (and (> length 7)
             (equal (substring filename -7 length) ".icloud")
             (eq (aref filename 0) ?.))
        (concat
         (file-name-directory path)
         (substring filename 1 -7))
      nil)))

(defun icloud-get-file (file &optional error)
  "Get FILE from iCloud, wait on its progress, return its name.
FILE should be the name of the file in the cloud.
If FILE doesn't exist yet, try downloading it.
If FILE doesn't exist after waiting for `icloud-max-wait', return nil.
If ERROR is non-nil, throw an error when download fails"
  (if (file-exists-p file)
      (progn
        (message "%s %s"
                 (icloud-propertize-message
                  "This file already exists:")
                 file)
        file)
    (progn
      (icloud-start-download file)
      (icloud-report-on-progress file error 'to-message))))

(defun icloud-get-file-if-cloud (file &optional error)
  "Get FILE from iCloud, wait on its progress, return name of local copy.
When file is a non-downloaded local copy of an iCloud file
\(eg \"my_dir/.my_file.txt.icloud\"),
convert it to its name in the cloud, and download it with `icloud-get-file'.
When FILE is not in this format, return its name (the argument FILE as given).
If ERROR is non-nil, throw an error when download fails"
  (let ((cloud-file (icloud-local-to-download file)))
    (if cloud-file
        (icloud-get-file cloud-file error)
      (progn
        (message "%s %s"
                 (icloud-propertize-message
                  "This file is not an \"*.icloud\" file:")
                 file)
        file))))

;;;###autoload
(defun icloud-find-file (file)
  "Open FILE, downloading it if needed.
This is intended for programmatic use: it expects a file-name
without trailing \".icloud\", so your program won't have to worry about
the file's name changing when the file is in the cloud.
For interactive use, see
`icloud-navigation-mode' and `icloud-open'."
  (find-file (icloud-get-file file)))

;;;; interactive interface for single-file downloads

;; TODO: is this function really needed?
;; seems that, when non-async is needed, user would just opt for `icloud-open'
;; and if we just need to download a file without immediately opening it,
;; why not use `icloud-async-download'
;;;###autoload
(defun icloud-download (file)
  "Download FILE from iCloud, wait on its progress, return name of local copy.
This function is to be used interactively.
For use in a program, see `icloud-start-download' or `icloud-get-file-if-cloud'"
  (interactive
   (list (read-file-name "icloud download: ")))
  (icloud-get-file-if-cloud file 'error))

;;;###autoload
(defun icloud-async-download (file)
  "Download FILE from iCloud. Report on its progress in an async thread."
  (interactive
   (list (read-file-name "icloud async download: ")))
  (let ((file (icloud-local-to-download file)))
    (icloud-start-download file)
    (make-thread
     (lambda ()
       (icloud-log
        nil
        "%s %s."
        (icloud-propertize-message "-----------\nDownloading file:") file)
       (icloud-report-on-progress file nil t)))))

;;;###autoload
(defun icloud-open (&optional function)
  "Temporarily activate `icloud-navigation-mode' and run FUNCTION to open files.
This is to leverage the completion capabilities and configuration of FUNCTION,
while `icloud-navigation-mode' ensures that the file is downloaded if needed.
When FUNCTION is nil, we use `icloud-default-open-function'.
FUNCTION should be an interactive function:
for opening cloud files within a program, see `icloud-find-file'"
  (interactive)
  (unwind-protect
      (progn
        (icloud-navigation-mode 1)
        (call-interactively (or function icloud-default-open-function)))
    (icloud-navigation-mode -1)))

;;;; always download file when needed

(defun adv/icloud-get-file-if-cloud (filename)
  "Function used by `icloud-navigation-mode' to advice `insert-file-contents'.
It ensures that, if FILENAME is in the cloud, we download it before opening it.
This is done by calling `icloud-get-file-if-cloud' while filtering arguments
to `insert-file-contents'"
  (let ((downloaded-file
         (icloud-get-file-if-cloud (car filename) 'error)))
    ;; apparently the buffer-name is set before `insert-file-contents',
    ;; so we have to rename it with the non-".icloud" name
    ;; TODO: fix this; maybe the download should happen earlier?
    ;; need to look into when the buffer-name is created
    (rename-buffer (file-name-nondirectory downloaded-file) 'unique)
    ;; `insert-file-contents' expects arguments in a list
    (list downloaded-file (cdr filename))))

;;;###autoload
(define-minor-mode icloud-navigation-mode
  "Minor mode to download iCloud files when opening non-downloaded local copies.
This uses `icloud-get-file-if-cloud', which converts any file with
trailing \".icloud\" into its expected format, and tries to download it.
This advises `insert-file-contents',
so for every file we open (interactively or not),
it will check if it needs to be downloaded, and if so, attempt a download.
For one-time interactive use, see `icloud-open'.
For non-interactive use, see `icloud-find-file'"
  :global t
  :lighter " iCloud"
  :group 'comm
  (if icloud-navigation-mode
      (advice-add 'insert-file-contents :filter-args
                  #'adv/icloud-get-file-if-cloud)
    (advice-remove 'insert-file-contents #'adv/icloud-get-file-if-cloud)))

;;;; download multiple files

;;;###autoload
(defun icloud-download-in-directory (&optional directory regexp recurse report)
  "Find all iCloud files matching REGEXP in DIRECTORY, and download them.
DIRECTORY defaults to `default-directory'; REGEXP defaults to an empty string.
If RECURSE is non-nil, also find files within directories inside DIRECTORY.
If REPORT is non-nil, start an async thread to check on download progress,
using the buffer *icloud-progress-log*.
To check if files are in the cloud, REGEXP is altered to include a beginning
\"\\..*\" and a trailing \".*\\.icloud\""
  (interactive)
  ;; set the appropriate values
  ;; TODO: is there a better approach to interactive/default arguments?
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
         (recurse
          (or recurse
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
    ;; log what we will download
    (icloud-log
     nil
     "%s %s\n%s %s \n%s %s %s %s."
     (icloud-propertize-message "-----------\nDownloading in directory:")
     directory
     (icloud-propertize-message "for regexp:") regexp
     ;; double-negate to ensure these are logged as booleans:
     ;; the first `not' converts to bool; the second restores its intended value
     (icloud-propertize-message "recursing?") (not (not recurse))
     (icloud-propertize-message "reporting?") (not (not report)))
    ;; download
    (icloud-download-files (if recurse
                               (directory-files-recursively directory regexp)
                             (directory-files directory 'full regexp))
                           report)))
;;;###autoload
(defun icloud-download-files (files &optional report)
  "Download FILES from the cloud, if they are non-downloaded local copies.
If REPORT is non-nil, start an async thread to check on download progress,
using the buffer *icloud-progress-log*."
  (let ((files (mapcar
                #'icloud-local-to-download
                files)))
    (shell-command
     (mapconcat #'icloud-shell-command files " && "))
    (when report
      (let ((thread
             (make-thread
              (lambda ()
                (mapc #'icloud-report-on-progress files)
                (icloud-log 'to-message
                            "%s %s\n"
                            (icloud-propertize-message
                             "iCloud report finished for all documents at thread:")
                            (current-thread))))))
        (icloud-log 'to-message "%s %s"
                    (icloud-propertize-message "Checking on progress at thread:")
                    thread)))
    (icloud-log 'to-message "%s %s files"
                (icloud-propertize-message "Downloading")
                (length files))))

;;;; copy/move files to cloud drive

;;;###autoload
(defun icloud-copy-to (&optional file destination)
  (interactive)
  (icloud--cp-or-mv "cp" file destination))

;;;###autoload
(defun icloud-move-to (&optional file destination)
  (interactive)
  (icloud--cp-or-mv "mv" file destination))

;;;###autoload
(defun icloud-symlink-to (&optional file destination)
  (interactive)
  (let* ((file
          (or file
              (read-file-name "Symlink __ to iCloud: " default-directory)))
         (destination
          (or destination
              (read-file-name (format "Link %s to: "
                                      (file-name-nondirectory file))
                              icloud-destination-directory)))
         (move-first
          (when (file-exists-p file)
            (y-or-n-p (format "%s exists locally, move it to Cloud first?"
                              file))))
         ;; (override
         ;;  (when (file-exists-p destination)
         ;;    (y-or-n-p (format "%s already exists, overwrite it with %s?"
         ;;                      destination file))))
         (destination (string-replace
                       " " "\\ "
                       (progn
                         (when move-first
                           (icloud--cp-or-mv "mv" file (file-name-directory
                                                        destination)))
                         destination)))
         (command (format "ln -s %s %s" destination file)))
    (message "executing command: %s" command)
    (shell-command command)))

(defun icloud--cp-or-mv (action &optional file destination)
  (let* ((prompt (concat action "__ to iCloud: "))
         (file
          (or file
              (read-file-name prompt default-directory)))
         (destination
          (or destination
              (read-directory-name "To directory: "
                                   icloud-destination-directory)))
         (destination (string-replace " " "\\ " destination))
         (command (format "%s %s %s" action file destination)))
    (message "executing command: %s" command)
    (shell-command command)))

(provide 'icloud)
;;; icloud.el ends here
