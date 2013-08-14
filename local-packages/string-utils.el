;; string-utils --- string utilities

(defun comment-lines (str begin-comment &optional end-comment line-len)
	(let ((lines (split-string str "\n")))
		(mapconcat 'identity
							 (mapcar #'(lambda (line)
													 (if (null end-comment)
															 (format "%s%s" begin-comment line)
														 (if (null line-len)
																 (format "%s%s%s" begin-comment line end-comment)
															 (format (format "%%s%%-%ds%%s" (- line-len (+ (length end-comment)
																																						 (length begin-comment))))
																			 begin-comment line end-comment))))
											 lines)
							 "\n")))

(provide 'string-utils)
