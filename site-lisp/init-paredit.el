;;; init-paredit.el --- paredit initialization and customization
;;; Commentary:
;;; Code:
(require 'paredit)

;; some of paredit default keybindings don't work in non-windowed modes
(unless (display-graphic-p)
  (define-key paredit-mode-map (kbd ",") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd ".") 'paredit-forward-slurp-sexp))

;; turn paredit on for all lispy modes
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)

(provide 'init-paredit)
;;; init-paredit.el ends here
