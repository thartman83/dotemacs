;;; init-programming.el --- Custom initializations for my programming environment
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'magit)
(require 'ggtags)

;;Always used c++ mode pren style
(add-hook 'c-mode-hook '(lambda () (c-set-style "ellemtel")))
(add-hook 'c++-mode-hook '(lambda () (c-set-style "ellemtel")))
(add-hook 'java-mode-hook '(lambda () (c-set-style "ellemtel")))

(add-hook 'c-mode-common-hook
    (lambda()
      (set 'c-cleanup-list '(scope-operator
           brace-elseif-braces
           empty-defun-braces
           defun-close-semi
           list-close-comma
           brace-catch-braces
           space-before-funcall))
      (set 'c-hanging-braces-alist '((substatement-open . after)))
      (set 'c-echo-syntactic-information-p t)
      (set 'c-tab-always-indent t)
      (set 'c-comment-only-line-offset 4)
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
        (ggtags-mode 1))))

;; Qt specific c++ language extensions

(font-lock-add-keywords 'c++-mode
      '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
(font-lock-add-keywords 'c++-mode
      '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
(font-lock-add-keywords 'c++-mode
                        '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))

;; Highlight merge conflicts
(defun smerge-try-smerge ()
  "If current buffer has merge markers turn `smerge-mode' on."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(defun comment-lines (str beg end line-width)
  "Return a commented version of STR using BEG, END and LINE-WIDTH."
  (let ((lines (split-string str "\n")))
    (mapconcat #'(lambda (line)
                   (concat beg " " str (make-string (- line-width
                                                       (length str)
                                                       (+ (length beg) 1)
                                                       (length end)) ? )
                           end)) lines "\n")))

(provide 'init-programming)
;;; init-programming.el ends here
