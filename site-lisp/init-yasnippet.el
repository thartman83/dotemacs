;;; init-yasnippet.el --- Initializations and customizations for yasnippet
;;; Commentary:
;;; Code:
(require 'yasnippet)

(yas-global-mode 1)
(setq yas/root-directory '("~/.emacs.d/snippets"))
(mapc #'yas-load-directory yas/root-directory)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
