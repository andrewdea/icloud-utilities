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
(defcustom icloud-max-wait 3
  "Integer denoting how many seconds `icloud-download' should wait.
If the file still isnt' downloaded after this amount of seconds,
error out."
  :type 'integer
  :group 'comm)

;;;###autoload
(defun icloud-download (file)
  "Download FILE from the cloud, using brctl.
Wait for `icloud-max-wait' seconds: if the file is still not found, error out."
  (let ((brctl-command (concat
                        "brctl download \""
                        file
                        "\"")))
    (shell-command brctl-command)
    (message "%s %s"
             (propertize "brctl download:" 'face 'minibuffer-prompt)
             file)
    (let ((start-time (current-time)))
      (while (and
              (not (file-exists-p file))
              (> icloud-max-wait (nth 1 (time-subtract nil start-time))))
        (sleep-for 0.005)))
    (if (file-exists-p file)
        (progn
          (message (propertize "Download succeeded" 'face 'minibuffer-prompt))
          file)
      (error (message
              "Could not download this file from iCloud: %s" file)))))

(defun icloud-download-if-needed (path)
  "Check if the file at PATH is in iCloud, and if it is, download it.
The check is by verifying that the file-name ends with \".icloud\"
and starts with a \".\". The download is done by `icloud-download'"
  (let* ((path (expand-file-name path))
         (filename (file-name-nondirectory path))
         (length (length filename)))
    (if (and (> length 7)
             (equal (substring filename -7 length) ".icloud")
             (eq (aref filename 0) ?.))
        (icloud-download (concat
                          (file-name-directory path)
                          (substring filename 1 -7)))
      path)))

(defun icloud-find-file (filename)
  "If FILENAME is non-existent, try `icloud-download'ing it.
Then, open it with `find-file'.
This is intended for programmatic use: it expects a FILENAME
without trailing \".icloud\", so your program won't have to worry about
the FILENAME changing when the file is in the cloud.
For interactive use, see `icloud-navigation-mode'."
  (if (file-exists-p filename)
      (find-file filename)
    (find-file (icloud-download filename))))

;; TODO: maybe `icloud-download-if-needed' should be broken down into 2 parts:
;; in this case, all the checks are useless, since we know the arguments we send
;; fit the bill
;; TODO: there must a better approach to interactive/default arguments?
;;;###autoload
(defun icloud-download-in-dir (&optional directory regexp recursively)
  "Find all iCloud files matching REGEXP in DIRECTORY, and download them.
If RECURSIVELY is non-nil, also find files within directories inside DIRECTORY.
To check they are in the cloud, REGEXP is altered to include a beginning
\"\\..*\" and a trailing \".*\\.icloud\""
  (interactive)
  (let* ((directory
          (or (when (called-interactively-p 'any)
                (read-directory-name
                 "Download iCloud files in this directory: "))
              directory default-directory))
         (regexp
          (or (when (called-interactively-p 'any)
                (read-string
                 "Download files matching regexp (empty means all files): "))
              regexp ""))
         (regexp (concat "\\..*" regexp ".*\\.icloud"))
         (recursively
          (or recursively
              (when (called-interactively-p 'any)
                (read-string
                 "Recursively? (empty means NO, anything else is YES) ")))))
    (message "downloading in directory %s for regexp %s, recursively: %s"
             directory regexp recursively)
    (mapc #'icloud-download-if-needed
          (if (> (length recursively) 0)
              (directory-files-recursively directory regexp)
            (directory-files directory regexp)))))

(defun adv/icloud-download-if-needed (filename)
  "Function used as advice to `insert-file-contents'.
It ensures that, if FILENAME is in the cloud, we download it before opening it.
This is done by calling `icloud-download-if-needed' while filtering arguments
to `insert-file-contents'"
  (let ((downloaded-file
         ;; apparently the buffer-name is set before `insert-file-contents',
         ;; so we have to rename it with the non-".icloud" name
         ;; TODO: fix this; maybe the download should happen earlier?
         ;; need to look into when the buffer-name is created
         (icloud-download-if-needed (car filename))))
    (rename-buffer (file-name-nondirectory downloaded-file) 'unique)
    (list downloaded-file (cdr filename))))

;;;###autoload
(define-minor-mode icloud-navigation-mode
  "Minor mode to download iCloud files when we try to open \".icloud\" files.
this uses `icloud-download-if-needed', which converts any file with
trailing \".icloud\" into its expected format, and tries to download it."
  :global t
  :lighter " icloud"
  :group 'comm
  (if icloud-navigation-mode
      (advice-add 'insert-file-contents :filter-args
                  #'adv/icloud-download-if-needed)
    (advice-remove 'insert-file-contents #'adv/icloud-download-if-needed)))

;;;; async support (somewhat experimental)
;; TODO: for better support of these, maybe it'd be better to run an async shell command?
;; but since it looks like we can't run brctl on multiple files,
;; either we create a buffer for each shell-command, or....????

;; TODO: I wonder if best practice around async processes is to use a specialized buffer
;; for their messages. in that case, we'd need to generate a buffer for each thread
;; and use a `icloud-log' function instead of message
;; `icloud-log' should check a buffer-local flag and write to the log, or to the message buffer
(defvar icloud-async-threads '()
  "List of threads running icloud jobs.
these threads are not guaranteed to be currently \"alive\": they are simply
the ones that were alive last time we checked. for an accurate list of
live icloud threads, run

\(setq icloud-async-threads
          (seq-intersection icloud-async-threads (all-threads)))")

;;;###autoload
(defun icloud-async-download-in-dir (&optional directory regexp recursively)
  "Make an asynchronous thread to run `icloud-download-in-dir'.
`icloud-download-in-dir' will:
Find all iCloud files matching REGEXP in DIRECTORY, and download them.
If RECURSIVELY is non-nil, also find files within directories inside DIRECTORY.
To check they are in the cloud, REGEXP is altered to include a beginning
\"\\..*\" and a trailing \".*\\.icloud\""
  (interactive)
  (let* ((directory
          (or (when (called-interactively-p 'any)
                (read-directory-name
                 "Download iCloud files in this directory: "))
              directory default-directory))
         (regexp
          (or (when (called-interactively-p 'any)
                (read-string
                 "Download files matching regexp (empty means all files): "))
              regexp ""))
         (recursively
          (or recursively
              (when (called-interactively-p 'any)
                (read-string
                 "Recursively? (empty means NO, anything else is YES) "))))
         (thread
          (make-thread
           (lambda ()
             (icloud-download-in-dir directory regexp recursively)))))
    (message "Downloading iCloud files at this thread: %s" thread)
    (setq icloud-async-threads
          (seq-intersection icloud-async-threads (all-threads)))
    (add-to-list 'icloud-async-threads thread)
    thread))

(defun icloud-stop-async-jobs ()
  "Stop all `icloud-async-threads'.
First, check which iCloud threads are alive.
Then temporarily set `icloud-max-wait' to 0.
This makes all the threads error out (see `icloud-download') and stop.
Wait until all `icloud-async-threads' are dead, then restore `icloud-max-wait'
and exit."
  (setq icloud-async-threads
        (seq-intersection icloud-async-threads (all-threads)))
  (when icloud-async-threads
    (let ((cache icloud-max-wait))
      (setq icloud-max-wait 0)
      (while (setq icloud-async-threads
                   (seq-intersection icloud-async-threads (all-threads)))
        (message "killing iCloud threads. Number of threads: %s"
                 (length icloud-async-threads))
        (sleep-for 0.05))
      (setq icloud-max-wait cache)))
  (message "there are no more iCloud threads"))

(provide 'icloud)
;;; icloud.el ends here
