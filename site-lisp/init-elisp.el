;;; init-elisp.el --- Initialize custom elisp mode bindings and functions
;;; Commentary:
;;; Code:

(eval-after-load "emacs-lisp-mode"
  (progn
    (define-key emacs-lisp-mode-map (kbd "C-c e")
      #'(lambda ()
         (interactive)
         (eval-buffer)
         (message "%s evaluated" (buffer-name (current-buffer)))))))

(provide 'init-elisp)

;;; init-elisp.el ends here
