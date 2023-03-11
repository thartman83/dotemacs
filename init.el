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
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
;;			 ("org" . "https://orgmode.org/elpa/")
			 ))

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

;;(use-package dash)

(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono-8"))

;;(use-package doom-themes
;;  :init (load-theme 'doom-sourcerer t))
(use-package doom-themes
  :init (load-theme 'doom-acario-dark t))

(defun set-transparency (value)
  "Set the transparency `VALUE' of the frame window 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100: ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Transparency needs to be set when a frame is created for cases where we are using emacsclient instead of a new instance
(defun new-frame-setup (frame)
  (when frame
    (select-frame frame))
  (when (display-graphic-p frame)
      (set-transparency 90)))

;; Run for already-existing frames
(mapc 'new-frame-setup (frame-list))

;; Run when a new frame is created
;;(add-hook 'before-make-frame-functions 'new-frame-setup)
(add-hook 'server-after-make-frame-hook 'new-frame-setup)

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package telephone-line
  :custom
  (telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-height 16)
  (telephone-line-mode 1))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package multiple-cursors
  :ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; no littering package handles a lot of emacs temp file mainenance in a nice way
(use-package no-littering)

;; keep customizations out of the init file
;;(setq custom-file
;;      (if (boundp 'server-socket-dir)
;;          (expand-file-name "custom.el" server-socket-dir)
;;        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;;(load custom-file t)

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode 1)
	    (display-line-numbers-mode 0)))

(add-hook 'org-mode-hook 'turn-on-flyspell)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
;;  :pin org
  :hook (org-mode . efs/org-mode-setup)
  :ensure org-contrib
  :bind ()
  :custom
  ;; Right justifies tags on headers adjusting for a default line width of 80
  (org-tags-column -80)
  (org-agenda-tags-column -80)
  :config
  (auto-fill-mode)
  (setq org-startup-folded "fold")
  (setq org-ellipsis " ▾")
  (setq org-return-follows-link t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; Prevent org-agenda from creating or clobbering frames
  (setf org-agenda-window-setup 'other-window)

  ;; Refile targets
  (setq org-refile-targets
        '(("~/notes/globals/punchlist.org" :maxlevel . 1)))
  )

(setf org-src-preserve-indentation t)

(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell .t)
			       (emacs-lisp . t)))

(setf org-src-window-setup 'other-window)

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

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setf org-agenda-window-setup 'other-window)

(setq org-agenda-custom-commands
      '(("d" "Default view of scheduled items and todos"
         ((agenda "")
          (tags-todo "Punchlist"
                     ((org-agenda-overriding-header "Punchlist")))
          (tags-todo "*"
                     ((org-agenda-overriding-header "All of the things")))
          ))))

(defun org-export-as-pdf-and-open ()
  (interactive)
  (save-buffer)
  (org-open-file (org-latex-export-to-pdf)))

(add-hook
 'org-mode-hook
 (lambda()
   (define-key org-mode-map
       (kbd "<f5>") 'org-export-as-pdf-and-open)))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/notes")
  (org-agenda-files '("~/notes/journal"
		      "~/notes/globals/"))
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S$>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("h" "house project" plain
      (file "~/.emacs.d/org-templates/house-project.org")
      :if-new (file+head "%<%Y%m%d%H%M%S$>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "recipe" plain
      (file "~/.emacs.d/org-templates/recipe.org")
      :if-new (file+head "%<%Y%m%d%H%M%S$>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind  (("C-c n l" . org-roam-buffer-toggle)
	  ("C-c n f" . org-roam-node-find)
	  ("C-c n i" . org-roam-node-insert)
	  :map org-mode-map
	  ("C-M-i"   . completion-at-point)
	  :map org-roam-dailies-map
	  ("Y" . org-roam-dailies-capture-yesterday)
	  ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package git-auto-commit-mode)

(use-package org-capture
  :ensure nil
  :after org
  :custom
  (org-capture-templates
   `(("p" "Punchlist Item" entry
      (file+headline "~/notes/globals/punchlist.org" "Punch list"),
      "* TODO %? %^G\n %i")
     ("e" "Event" entry
      (file+headline "~/notes/globals/calendar.org" "Events"),
      "* %? %^G\nSCHEDULED: %^t\n %i")
     ("w" "Whereabouts" entry
      (file+headline "~/notes/globals/calendar.org" "Whereabouts")
      "* %? \n SCHEDULED: %^t\n %i")
     ("a" "Appointment" entry
      (file+headline "~/notes/globals/calendar.org" "Appointments")
      "* %? %^G\n SCHEDULED: %^t\n %i")
     (("P" "Project")
     ))))

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

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq tab-width 2)
  (add-to-list 'lsp-enabled-clients 'jsts-ls))

(use-package json-mode
  :hook (json-mode . lsp-deferred)
  :config
  (add-to-list 'lsp-enabled-clients 'json-ls)
  (setq tab-width 2))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(defun tlh/enable-lsp-on-pyvenv ()
  "Enable lsp mode after pyvenv activation"
  (when (null (getenv "VIRTUAL_ENV"))
    (call-interactively 'pyvenv-activate))
  (lsp-deferred))

(use-package python-mode
  :ensure t
  :hook ((python-mode . tlh/enable-lsp-on-pyvenv)
         (flycheck-mode . (lambda ()
                            (flycheck-add-next-checker 'lsp 'python-flake8))))
  :custom
  (lsp-pylsp-plugins-pylint-enabled t))

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
  (add-to-list 'lsp-enabled-clients 'lsp-lua-lsp)
  (setf lsp-clients-lua-lsp-server-install-dir "~/.luarocks/bin/lua-lsp"))

(use-package emmet-mode
  :hook (mhtml-mode . emmet-mode))

(use-package yaml-mode)

;;(lsp-install-server 'css-ls)

(use-package scss-mode
  :mode "\\.scss'"
  :hook (scss-mode . lsp-deferred)
  :config
  (setq scss-indent-level 2)
  (add-to-list 'lsp-enabled-clients 'css-ls))

(use-package origami
  :config
  (global-origami-mode))

(use-package yaml-mode)

(use-package skeletor)

(use-package dockerfile-mode)

(use-package docker-compose-mode
  :bind ("C-c D" . docker-compose))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "'" "'" :actions nil))

(use-package yasnippet
  :custom
  (yas/root-directory '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1)
  (mapc #'yas-load-directory yas/root-directory))

(use-package restclient)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((gac-automatically-push-p . t)
     (gac-automatically-add-new-files-p . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
