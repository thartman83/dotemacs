;;; init-slime.el --- customizations for slime
;;; Commentary:
;;; Code:
(require 'slime)

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-hook 'lisp-mode-hook '(lambda () (slime-mode)))
(slime-setup '(slime-repl slime-fuzzy))

(provide 'init-slime)
;;; init-slime.el ends here
