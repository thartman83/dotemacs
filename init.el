;;; init.el --- Emacs configuration and initialization file

;; Copyright (c) 2020 Thomas Hartman (thomas.lees.hartman@gmail.com)

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
;; This is my Emacs configuration file.There are many like it but this one is mine.
;;
;; There is a lot of code in here that was lifted or modified from
;; other sources.Below is a non-exhaustive list of them:
;;
;; System Crafters Emacs from Scratch: https://github.com/daviwil/emacs-from-scratch
;;

;;

;;; Code:

 (menu-bar-mode -1)
 (scroll-bar-mode -1)
 (tool-bar-mode -1)

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
;;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq startup-screen-inhibit-startup-screen t)

     ;; Initialize package sources
     (require 'package)

     (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			      ("org" . "https://orgmode.org/elpa/")
			      ("elpa" . "https://elpa.gnu.org/packages/")))

     (package-initialize)
     (unless package-archive-contents
       (package-refresh-contents))

     ;; Initialize use-package on non-Linux platforms
     (unless (package-installed-p 'use-package)
       (package-install 'use-package))

     (require 'use-package)
     (setq use-package-always-ensure t)

     (use-package auto-package-update
	:ensure t
	:config
	(setq auto-package-update-delete-old-versions t
	      auto-package-update-interval 4)
	(auto-package-update-maybe))

(defvar *full-name* "Tom Hartman")
(defvar *email* "thomas.lees.hartman@gmail.com")

(defun tlh/comment-lines (str beg end line-width)
  "Return a commented version of STR using BEG, END and LINE-WIDTH."
  (let ((lines (split-string str "\n")))
    (mapconcat #'(lambda (line)
                   (concat beg " " str (make-string (- line-width
                                                       (length str)
                                                       (+ (length beg) 1)
                                                       (length end)) ? )
                           end)) lines "\n")))

(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono-8"))

(use-package doom-themes
  :init (load-theme 'doom-sourcerer t))

(defun set-transparency (value)
  "Set the transparency `VALUE' of the frame window 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100: ")

  (set-frame-parameter (selected-frame) 'alpha value))
   
(set-transparency 90)

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

(use-package multiple-cursors
  :ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t))

   (setf org-src-preserve-indentation t)

 (require 'org-tempo)
  
 (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
 (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
 (add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))



(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package flycheck)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects/")
    (setq projectile-project-search-path '("~/projects/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package paredit
  :config
  
  ;; slurping in a terminal doesn't quite work, so rebind keys so they do
  (unless (display-graphic-p)
    (define-key paredit-mode-map (kbd ",") 'paredit-backward-slurp-sexp)
    (define-key paredit-mode-map (kbd ".") 'paredit-forward-slurp-sexp))

  ;; turn paredit on for all lispy modes
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)

  ;; turn on paredit for Cask files too
  (add-to-list 'auto-mode-alist '("Cask" . paredit-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'lsp-enabled-clients 'ts-ls))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
 ;; :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
;;  (dap-python-debugger 'debugpy)
;;  :config
;;  (require 'dap-python)
  )

(use-package pyvenv)
(pyvenv-activate "~/venv/")

(use-package jedi)

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package scad-mode)
(use-package scad-preview
  :mode "\\.scad\\'"
  :custom
  (scad-preview-image-size '(900 . 900))
  :config
  (defun scad-export-stl ()
    "Exports the current visited filename as an stl file."
    (interactive)
    (call-process "openscad" nil "*openscad-output*" t
                  "-o" (f-swap-ext (f-filename (buffer-file-name)) "stl")
                  (buffer-file-name))))

(use-package lua-mode
  :hook (lua-mode . lsp-deferred)
  :config
  (add-to-list 'lsp-enabled-clients 'lsp-lua-lsp))

(use-package emmet-mode
  :hook (mhtml-mode . emmet-mode))

(use-package origami
  :config
  (global-origami-mode))

(use-package yasnippet
  :custom
  (yas/root-directory '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1)
  (mapc #'yas-load-directory yas/root-directory))

(use-package restclient)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode all-the-icons-dired restclient yasnippet which-key visual-fill-column use-package typescript-mode slime scad-preview pyvenv python-mode paredit origami org-bullets multiple-cursors mixed-pitch lua-mode lsp-ui lsp-jedi jedi ivy-rich forge flycheck emmet-mode doom-themes dap-mode counsel-projectile company-box auto-package-update async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
