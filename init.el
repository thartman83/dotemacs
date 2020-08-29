;;; init.el --- My emacs initialization
;;; Commentary:
;;
;;; Code:

(package-initialize)

(require 'cask "/usr/share/emacs/site-lisp/cask/cask.el")
(cask-initialize "~/.emacs.d/")
(require 'pallet)
(pallet-mode t)

;; Setup initialization paths
(defvar site-lisp-dir (f-expand (f-join user-emacs-directory "site-lisp"))
  "Path where setup and configuration files for Emacs reside.")
(add-to-list 'load-path site-lisp-dir)

;; Custom File - Contains custom variables that are not maintained by git
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load personal information and vars that are not maintained by git
(require 'personal)

;; Load the rest of the initialization files from site-lisp
(require 'init-auto-complete)
(require 'init-yasnippet)
(require 'init-slime)
(require 'init-elisp)
(require 'init-paredit)
(require 'init-ielm)
(require 'init-skeletor)
(require 'init-flycheck)
(require 'init-ggtags)
(require 'init-helm)
(require 'init-mc)
(require 'init-org)
(require 'init-org-fc)
(require 'init-origami)
(require 'init-magit)
(require 'init-programming)
(require 'init-projectile)
(require 'init-treeview)
(require 'init-ledger)
(require 'init-appearance)

;;; init.el ends here
