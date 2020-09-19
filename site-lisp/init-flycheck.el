;;; init-flycheck.el --- Initialization and customization of flycheck
;;; Commentary:
;;; Code:
(require 'flycheck)
(require 'elpy)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda ()
            (setf flycheck-clang-language-standard "c++11")))

;; ;; This is to avoid load-path issues for local .el work
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
