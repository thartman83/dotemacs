;; file-utils.el --- file utilities

(defun read-file-contents (filename)
  "Reads the contents of filename and returns it as a lisp form.
   Returns nil if the file does not exist."
  (if (file-exists-p filename)
      (with-temp-buffer (insert-file-contents filename)
                        (read (buffer-substring (point-min) (point-max))))
    nil))

(defun write-file-contents (str filename)
  "Write the string str as the contents of filename, overwriting any data that
   it may have already contained."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (kill-region (point-min) (point-max))
      (insert str)
      (save-buffer)
      (kill-buffer))))

(defun file-contents (filename)
	(with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; Directory Utilities, mostly in service to the with-temporary-dir macro

(defun get-new-dir-name (path)
  "Returns an unused random directory name in path."
  (let ((rand-str (get-random-string 6)))
    (if (not (file-exists-p (file-name-as-directory (concat path rand-str))))
        (file-name-as-directory (concat path rand-str))
      (get-random-dir path))))

(defun get-random-string (length)
  (let ((chars "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))   
    (if (= length 1)
        (string (elt chars (random (length chars))))
      (concat (string (elt chars (random (length chars)))) (get-random-string (1- length))))))

(defun append-string-into-file (str filename)
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (goto-char (point-max))
      (insert str)
      (save-buffer)
      (kill-buffer))))

(defmacro with-temporary-dir (&rest body)
  "Create a temporary directory in pwd and cd's into it. Removes directory
    and its contents at the end of execution. Returns the value of body."
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
	`(let ((olddir default-directory))
		 (unwind-protect
				 ,@body
			 (cd olddir))))

(provide 'file-utils)
