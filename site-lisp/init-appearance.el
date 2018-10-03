;;; init-appearance.el --- custom appearances and what not
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;
;; Emacs Window ;;
;;;;;;;;;;;;;;;;;;

;; Hulk smash puny scroll and menu bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;;;;;;;;;;;;
;; Scratch ;;
;;;;;;;;;;;;;

;; Go directly to scratch buffer, do not pass go
(setq startup-screen-inhibit-startup-screen t)
(setq initial-scratch-message nil)
(put 'erase-buffer 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;
;; Custom Mode Line ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Custom Mode line, mostly stolen from:
;; https://www.cs.elte.hu/local/texinfo/elisp-intro/emacs-lisp-intro_205.html
;; Grabs and formats current computer name
(defvar tlh/mode-line-system-identification
  (substring (system-name) 0 (string-match "\\..+" (system-name))))

;; Sets the mode line to be:
;; < Computer name> Current Buffer (Major-Mode,minor-modes)--Line X,Col Y--Prec
(defvar tlh/default-mode-line-format
      (list "" 'mode-line-modified "< " 'tlh/mode-line-system-identification ":"
      " >" " %6b " "%[(" 'mode-name 'minor-mode-alist "%n"
      'mode-line-process ")%]--" "Line %l,Col %c--" '(-3 . "%P") "-%-"))

;; Set it to be the current buffer mode line format
(setq mode-line-format tlh/default-mode-line-format)

;;;;;;;;;;;;;
;; Tabbing ;;
;;;;;;;;;;;;;
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'makefile-gmake-mode)
              (untabify (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;
;; Parenthesis ;;
;;;;;;;;;;;;;;;;;
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;
;; Coloring & Fonts ;;
;;;;;;;;;;;;;;;;;;;;;;

;;; (set-default-font "-windows-proggytiny-medium-r-normal--10-80-96-96-c-60-iso8859-1")

(load-theme 'cyberpunk)
(add-to-list 'default-frame-alist '(font . "proggytiny-10"))


;;;;;;;;;;;;;;;;;;
;; Transparency ;;
;;;;;;;;;;;;;;;;;;
(defun set-transparency (value)
  "Set the transparency `VALUE' of the frame window 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(set-transparency 80)

(provide 'init-appearance)
;;; init-appearance.el ends here
