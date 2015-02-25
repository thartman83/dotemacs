;;; init.el --- My emacs initialization
;;; Commentary:
;;
;;; Code:

;; Get the package manager up and running and load all neccessary require
;; packages if they aren't already there
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages nil "A list of required packages for initialization.")

(setq required-packages
      '(auto-complete
        f
        flycheck
        cyberpunk-theme
        ggtags
        helm
        helm-gtags
        helm-flycheck
        helm-google
        ht
        lua-mode
        magit
        multiple-cursors
        org
        paredit
        semantic
        slime
        yasnippet))

(defun tlh/install-all-required-packages ()
  "Install all required packages."
  (interactive)
  (dolist (package required-packages)
    (unless (package-installed-p package)
      (package-install package))))

(tlh/install-all-required-packages)

(require 'f)
;; Setup initialization paths
(defvar site-lisp-dir (f-expand (f-join user-emacs-directory "site-lisp"))
  "Path where setup and configuration files for Emacs reside.")

(add-to-list 'load-path site-lisp-dir)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load personal information and vars
(require 'personal nil t)

;; Load the rest of the initialization files from site-lisp
(require 'init-auto-complete)
(require 'init-yasnippet)
(require 'init-slime)
(require 'init-elisp)
(require 'init-paredit)
(require 'init-ielm)
(require 'init-flycheck)
(require 'init-ggtags)
(require 'init-helm)
(require 'init-org)
(require 'init-programming)
(require 'init-appearance)

;;; init.el ends here
