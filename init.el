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
	;; :config
	;; (setq auto-package-update-delete-old-versions t
	;;       auto-package-update-interval 4)
	;;(auto-package-update-maybe)
  )

(defvar tlh/fullname "Tom Hartman")
(defvar tlh/email "thomas.lees.hartman@gmail.com")

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

(use-package f
  :ensure t)

(defun str-in-file-p (needle file)
  "Returns t if string `needle' exists in `file'."
  (and (f-exists-p file)
       (integerp (string-match-p needle (f-read-text file 'utf-8)))))

(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono-10"))

;;(use-package doom-themes
;;  :init (load-theme 'doom-sourcerer t))
(use-package doom-themes
  :init (load-theme 'doom-acario-dark t))

 ;; Set transparency of emacs
 (defun set-transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha-background value))

;; Transparency needs to be set when a frame is created for cases where we are using emacsclient instead of a new instance
(defun new-frame-setup (frame)
  (message "in new frame setup")
  (when frame
    (select-frame frame))
  (when (display-graphic-p frame)
      (set-transparency 80)))

;; Run for already-existing frames
;(mapc 'new-frame-setup (frame-list))

;; Run when a new frame is created
;;(add-hook 'before-make-frame-functions 'new-frame-setup)
;;(add-to-list 'after-make-frame-functions #'new-frame-setup)

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

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-suppress-auto-error t)
  (set-face-foreground 'highlight-indent-guides-character-face "#222222")
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)

(use-package multiple-cursors
  :ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package beacon
:init (beacon-mode 1))

(use-package guru-mode
  :hook (prog-mode . guru-mode)
  :config
  (setq guru-warn-only t))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(setq projectile-known-projects-file
      (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(global-set-key (kbd "C-c =") 'calc)

(global-set-key (kbd "C-c i")
                #'(lambda ()
                    (interactive)
                    (when (window-in-direction 'up)
                      (select-window (window-in-direction 'up)))))

(global-set-key (kbd "C-c m")
                #'(lambda ()
                    (interactive)
                    (when (window-in-direction 'down)
                      (select-window (window-in-direction 'down)))))

(global-set-key (kbd "C-c j")
                #'(lambda ()
                    (interactive)
                    (when (window-in-direction 'left)
                      (select-window (window-in-direction 'left)))))

(global-set-key (kbd "C-c l")
                #'(lambda ()
                    (interactive)
                    (when (window-in-direction 'right)
                      (select-window (window-in-direction 'right)))))

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

(use-package substitute)

(add-hook 'org-mode-hook 'turn-on-flyspell)

(defun tlh/autoload-python-venv ()
  (message (concat "Loading pyvenv " (projectile-project-root) "venv"))
  (when (string= projectile-project-type "python-pip")
    (pyenv-activate (concat (projectile-project-dir) "venv"))))

(use-package projectile
  :diminish projectile-mode
  :config
  (add-hook 'projectile-after-switch-project-hook #'tlh/autoload-python-venv)
  (projectile-mode)

  ;; detect fastapi projects
  (projectile-register-project-type
   'python-fastapi
   #'(lambda (project-root)
       (str-in-file-p "fastapi" (concat project-root "requirements.txt")))
   :project-file "requirements.txt"
   :compile "uvicorn app.main:app --reload"
   :test ""
   :run "uvicorn app.main:app --reload"
   :test-prefix "test_")

  ;; detect platformio projects (esp32 development)
  (projectile-register-project-type
   'platformio '("platformio.ini" "src" "lib" "test")
   :project-file "platformio.ini"
   :compile "pio run"
   :test "pio test"
   :src-dir '("src/" "lib/")
   :test-dir '("test"))

  (projectile-register-project-type
   'vite-storybook
   #'(lambda (project-root)
       (and (file-exists-p (concat project-root "vite.config.ts"))
            (file-exists-p (concat project-root ".storybook"))))
   :project-file '("README.org")
   :src-dir "src"
   :test-dir ".storybook"
   :compile "npx vite"
   :test "npm run storybook"
   :run "npx vite")

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

(use-package skeletor)

(defconst tlh/notes-dir
  "~/notes/"
  "Default notes directory. This is where the bulk of my org files are located in one form or another.")

(defconst tlh/project-notes-dir
  "~/notes/projects/"
  "Default location where project related notes are stored. This keeps them outside of the project space and allows for better capture options in conjunction with projectile."
  )

(defconst tlh/global-notes-dir
  "~/notes/globals/"
  "Catch all location of notes files: calendar, punchlist, etc."
  )

(defconst tlh/journal-dir
  "~/notes/journal/"
  "Location for org-roam to store daily notes.")

(defconst tlh/org-templates-dir
  (concat user-emacs-directory "org-templates/")
  "Location for all org-templates.")

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
  (org-directory tlh/notes-dir)
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
        '(((concat tlh/global-notes-dir "punchlist.org") :maxlevel . 1)))
  )

(setf org-src-preserve-indentation t)

(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell . t)
			       (emacs-lisp . t)
             (R . t)))
;             (yaml . t)))
(setf org-auto-load-images t)
(setf org-src-window-setup 'other-window)

 (require 'org-tempo)

 (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
 (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
 (add-to-list 'org-structure-template-alist '("py" . "src python"))
 (add-to-list 'org-structure-template-alist '("lu" . "src lua"))
 (add-to-list 'org-structure-template-alist '("yml" . "src yaml :tangle main.yml"))

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
  (org-roam-directory tlh/notes-dir)
  (org-agenda-files `(,tlh/project-notes-dir
                      ,tlh/global-notes-dir))
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
     ("t" "Ticket" entry
      (file+headline "~/nodes/globals/tickets.org" "Tickets"),
      " * SUBMITTED %? %^G\n %T\n%i")


     ;(("P" "Project"))
     )))

(use-package org-make-toc)

(defun tlh/org-project-file-path (project-name)
  "Returns the full path of the project related org file."
  (concat tlh/project-notes-dir project-name ".org"))

(defun tlh/org-project-file-p (project-name)
  "Returns t if `project-name`.org exists in the notes project directory."
  (file-exists-p (tlh/org-project-file-path project-name)))

(defun tlh/org-generate-project-notes-file (project-name)
  "Generates a default project org file in the notes project directory."
  (when (not (tlh/org-project-file-p project-name))
    (f-touch (tlh/org-project-file-path project-name))
    (with-temp-file (tlh/org-project-file-path project-name)
      (insert (f-read (concat tlh/org-templates-dir "project-notes.org")))
      (let ((substitute-fixed-letter-case t))
        (substitute-target-in-buffer "%%TITLE%%" project-name)
        (substitute-target-in-buffer "%%AUTHOR%%" tlh/fullname)))))

(defun tlh/org-capture-project ()
  "Capture project related notes and items.

Automatically files the target in the project specific org file
based on the current project of the calling buffer."
  (interactive)
  (let ((project-root (projectile-project-p))
        (project-name (projectile-project-name)))
    (when (not project-root)
      (error "Not in a project file buffer."))

    ;; Generate a default notes file if one doesn't already exist
    (when (not (tlh/org-project-file-p project-name))
      (tlh/org-generate-project-notes-file project-name))

    ;; shadow the existing capture templates with project specific
    ;; ones to be filed in the projects notes
    (let ((org-capture-templates
           `(("t" "Todo" entry
              (file+headline ,(tlh/org-project-file-path project-name) "Tasks")
              "* TODO %^{TaskTItle}\n %?\n")
             ("b" "Bug" entry
              (file+headline ,(tlh/org-project-file-path project-name) "Issues")
              "* BUG %^{BugTitle}\n %?\n")
             ("n" "Note" entry
              (file+headline ,(tlh/org-project-file-path project-name) "Notes")
              "* %^{NoteTitle}\n %?\n"))))
      (org-capture))))

(defun tlh/org-open-project-org ()
  (interactive)
  (let ((project-root (projectile-project-p))
        (project-name (projectile-project-name)))
    (when (not project-root)
      (error "Not in a project file buffer."))

    ;; Generate a default notes file if one doesn't already exist
    (when (not (tlh/org-project-file-p project-name))
      (tlh/org-generate-project-notes-file project-name))

    (find-file (tlh/org-project-file-path project-name))))




;; C-c p n appears to be free
(define-key projectile-command-map (kbd "n") 'tlh/org-capture-project)
(define-key projectile-command-map (kbd "N") 'tlh/org-open-project-org)

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

(use-package ivy-posframe
  :config
  (ivy-posframe-mode 1))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(require 'notifications)
(defun message-compilation-finished ()
  (notifications-notify
   :title "Compilation"
   :body "Compilation finished"))

(add-hook 'compilation-finish-functions #'message-compilation-finished 'compilation)

(use-package flycheck)

(defun efs/lsp-mode-setup ()
  (let ((lsp-keymap-prefix "C-c l"))
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode)
    (lsp-enable-which-key-integration)))

(use-package lsp-mode
  :load-path "~/projects/lsp-mode/"
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . efs/lsp-mode-setup)
         (tsx-ts-mode
          typescript-ts-mode
          js-ts-mode) . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c s")  ;; Or 'C-l', 's-l'
  (setq lsp-apply-edits-after-file-operations nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(defun os/setup-install-grammars ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
             (bash "https://github.com/tree-sitter/tree-sitter-bash")
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
             (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             (make "https://github.com/alemuller/tree-sitter-make")
             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
             (cmake "https://github.com/uyha/tree-sitter-cmake")
             (c "https://github.com/tree-sitter/tree-sitter-c")
             (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
             (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(defun tlh/associate-treesit-modes ()
  (interactive)
  (dolist (mapping
           '(;; (python-mode . python-ts-mode)
             ;; (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             ;; (c-mode . c-ts-mode)
             ;; (c++-mode . c++-ts-mode)
             ;; (c-or-c++-mode . c-or-c++-ts-mode)
             ;; (bash-mode . bash-ts-mode)
             ;; (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             ;; (sh-mode . bash-ts-mode)
             ;; (sh-base-mode . bash-ts-mode)
             ))
    (add-to-list 'major-mode-remap-alist mapping)))

(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-node-setup)
  :custom
  (bind-keys :prefix "C-c d" :prefix-map debug-keymap
             ("t" . dap-breakpoint-toggle)
             ("n" . dap-next)
             ("s" . dap-step-in)
             ("S" . dap-step-out)
             ("c" . dap-continue)
             ("r" . dap-restart)
             ("R" . dap-ui-repl)
             ("d" . dap-debug)))

  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  ;;:config
  ;; Set up Node debugging
  ;;(require 'dap-node)
  ;;(dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  ;;(general-define-key
  ;;  :keymaps 'lsp-mode-map
  ;;  :prefix lsp-keymap-prefix
  ;;  "d" '(dap-hydra t :wk "debugger")))

(use-package dap-chrome
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

  ;; turn on paredit for Cask files too
  (add-to-list 'auto-mode-alist '("Cask" . paredit-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . lsp-deferred)
         (typescript-mode . prettify-symbols-mode))
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'lsp-enabled-clients 'ts-ls))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq tab-width 2)
  (add-to-list 'lsp-enabled-clients 'jsts-ls))

;; (use-package tsx-ts-mode
;;   :mode ("\\.tsx\\'")
;;   :hook (((tsx-ts-mode
;;             typescript-ts-mode
;;             js-ts-mode) . lsp-deferred))

;;          ;; (rjsx-mode . lsp-deferred)
;;          ;; (rjsx-mode . emmet-mode))
;;   :config
;;   (setq tab-width 2)
;;   (setq js-indent-level 2)
;;   (add-to-list 'lsp-enabled-clients 'ts-ls))

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; (add-hook 'tsx-ts-mode 'lsp-deferred)
;; (add-hook 'typescript-ts-mode 'lsp-deferred)
;; (add-hook 'js-ts-mode 'lsp-deferred)

;; (add-to-list 'lsp-enabled-clients 'ts-ls)

;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection "mdx-language-server")
;;     :activation-fn (lsp-activate-on "mdx")
;;     :server-id 'mdx-language-server)))

(use-package mdx-mode
  :load-path "local/"
  :mode ("\\.mdx\\'")
  :hook ((mdx-mode . lsp-deferred)
         (mdx-mode . emmet-mode))
  :config
  (add-to-list 'lsp-enabled-clients 'mdx-analyzer))

(use-package json-mode
  :hook (json-mode . lsp-deferred)
  :config
  (add-to-list 'lsp-enabled-clients 'json-ls)
  (setq js-indent-level 2)
  (setq tab-width 2))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

(use-package python-mode
  :ensure t
  :hook ((python-mode . lsp-deferred)
         (flycheck-mode . (lambda ()
                            (flycheck-add-next-checker 'lsp 'python-flake8))))
  :custom
  (add-to-list 'lsp-enabled-clients 'pylsp)
  (setf lsp-pylsp-plugins-pylint-enabled t)
  :config
  (require 'dap-python))

(use-package pytest
  :bind (:map python-mode-map
              ("C-c C-t a" . pytest-all)
              ("C-c C-t m" . pytest-module)
              ("C-c C-t ." . pytest-one)
              ("C-c C-t c" . pytest-again)
              ("C-c C-t d" . pytest-directory)
              ("C-c C-t pa" . pytest-pdb-all)
              ("C-c C-t m" . pytest-pdb-module)
              ("C-c C-t p." . pytest-pdb-one)))

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

(use-package css-mode
  :mode "\\.css'"
  :hook (css-mode . lsp-deferred)
  :config
  (setq css-indent-offset 4)
  (add-to-list 'lsp-enabled-clients 'css-ls))

;;(lsp-install-server 'css-ls)

(use-package scss-mode
  :mode "\\.scss'"
  :hook (scss-mode . lsp-deferred)
  :config
  (setq scss-indent-level 4)
  (add-to-list 'lsp-enabled-clients 'css-ls))

(use-package platformio-mode
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pio\\'"))

(use-package ccls
  :hook (c-mode . lsp-deferred)
  :config
  (add-to-list 'lsp-enabled-clients 'ccls))

(use-package origami
  :config
  (global-origami-mode))

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package hcl-mode
  :mode "\\.tf\\'")

(use-package dockerfile-mode)

(use-package docker-compose-mode
  :bind ("C-c D" . docker-compose))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :custom
  (kubernetes-commands-display-buffer-function 'display-buffer)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "'" "'" :actions nil))

(use-package treemacs
  :ensure t
  :defer t
  :bind (("C-c w" . treemacs-select-window))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (setf lsp-treemacs-error-list-expand-depth 4)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook ((prog-mode) . yas-minor-mode)
  :bind (("C-<tab>" . yas-expand)
         :map yas-keymap
         ("C-<tab>" . yas-next-field-or-maybe-expand))
  :config
  (progn
    (add-to-list 'yas/root-directory '"~/.emacs.d/snippets")
    (yas-reload-all)))

(use-package restclient)

(use-package evil
; :config
;  (evil-mode 1)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dap-chrome yasnippet ws-butler which-key visual-fill-column typescript-mode treemacs-projectile treemacs-icons-dired terraform-mode telephone-line substitute smartparens skeletor scss-mode scad-preview restclient pyvenv python-mode pytest platformio-mode paredit origami org-roam org-make-toc org-contrib org-bullets no-littering multiple-cursors mixed-pitch lua-mode lsp-ui kubernetes json-mode js2-mode ivy-rich ivy-posframe highlight-indent-guides guru-mode git-commit git-auto-commit-mode forge flycheck evil emmet-mode doom-themes dockerfile-mode docker-compose-mode docker dap-mode counsel-projectile company-box ccls beacon auto-virtualenv auto-package-update all-the-icons-dired))
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
