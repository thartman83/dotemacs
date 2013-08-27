;; file-utils.el --- file utilities

;; Copyright (c) 2013 Thomas Hartman (rokstar83@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;;; Code:

(defun read-file-contents (filename)
  "Read the contents of FILENAME, return as a Lisp form.
Returns nil if the file does not exist."
  (if (file-exists-p filename)
      (with-temp-buffer (insert-file-contents filename)
                        (read (buffer-substring (point-min) (point-max))))
    nil))

(defun write-file-contents (str filename)
  "Write STR to FILENAME, overwrite data that it may have already contain."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (kill-region (point-min) (point-max))
      (insert str)
      (save-buffer)
      (kill-buffer))))

(defun file-contents (filename)
	"Return the contents of FILENAME as a string."
	(with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; Directory Utilities, mostly in service to the with-temporary-dir macro

(defun get-new-dir-name (path)
  "Return an unused random directory name in PATH."
  (let ((rand-str (get-random-string 6)))
    (if (not (file-exists-p (file-name-as-directory (concat path rand-str))))
        (file-name-as-directory (concat path rand-str))
      (get-random-dir path))))

(defun get-random-string (length)
	"Return a random string of letters and number of size LENGTH."
  (let ((chars "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (if (= length 1)
        (string (elt chars (random (length chars))))
      (concat (string (elt chars (random (length chars)))) (get-random-string (1- length))))))

(defun append-string-into-file (str filename)
	"Append STR to FILENAME."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (goto-char (point-max))
      (insert str)
      (save-buffer)
      (kill-buffer))))

(defmacro with-temporary-dir (&rest body)
  "Create a temporary directory in pwd and execute BODY in pwd.
Removes directory and its contents at the end of execution.  Returns the value of body."
  (let ((olddir default-directory)
        (dir (get-new-dir-name default-directory)))
    `(unwind-protect
         (progn
           (make-directory ,dir)
           (cd ,dir)
           ,@body)
       (progn (cd ,olddir)
              (when (file-exists-p ,dir)
               (delete-directory ,dir t))))))

(defmacro save-current-directory (&rest body)
	"Preserve PWD while executing BODY.
Any change in directory during the course of executing BODY is reverted at the
end of the block."
	`(let ((olddir default-directory))
		 (unwind-protect
				 ,@body
			 (cd olddir))))

(provide 'file-utils)

;;; file-utils.el ends here
