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

(defun generate-c++-configure.ac (project path)
  "Generate a default configure.ac for PROJECT to PATH/configure.ac."
  (interactive "sProject name: \nDConfigure.ac path:  ")
  (let ((downcase-project (s-downcase (s-replace "-" "_" project)))
        (upcase-project (s-upcase (s-replace "-" "_" project))))
    (f-write-text
     (mapconcat #'identity
                (list
                 "dnl Process this file with autoconf to produce a configure script."
                 ""
                 "AC_PREREQ(2.60)"
                 ""
                 (format "m4_define([%s_major_version], [0])" downcase-project)
                 (format "m4_define([%s_minor_version], [1])" downcase-project)
                 (format "m4_define([%s_version], \\" downcase-project)
                 (format "          [%s_major_version.%s_minor_version])"
                         downcase-project downcase-project)
                 ""
                 (format "AC_INIT([%s],[1.0])" downcase-project)
                 "AC_CONFIG_MACRO_DIR([config])"
                 "AM_INIT_AUTOMAKE([1.11 dist-bzip2])"
                 "LT_PREREQ([2.2])"
                 "LT_INIT([dlopen])"
                 ""
                 (format "AC_SUBST(%s_MAJOR_VERSION, [%s_major_version])"
                         upcase-project downcase-project)
                 (format "AC_SUBST(%s_MINOR_VERSION, [%s_minor_version])"
                         upcase-project downcase-project)
                 (format "AC_SUBST(%s_VERSION, [%s_version])"
                         upcase-project downcase-project)
                 ""
                 "dnl Check for programs."
                 ""
                 "AC_PROG_MAKE_SET"
                 "AC_PROG_INSTALL"
                 "AC_PROG_CXX"
                 "AC_LANG(C++)"
                 "AC_PROG_LIBTOOL"
                 "AC_LTDL_DLLIB"
                 ""
                 "dnl C++ checks"
                 "AC_CHECK_HEADERS"
                 "AX_CXX_COMPILE_STDCXX_11"
                 
                 "AC_CONFIG_FILES ([Makefile] [src/Makefile] [tests/Makefile])"
                 ""
                 "AC_OUTPUT")
                "\n")
     'utf-8 (f-join path "configure.ac"))))

(provide 'init-programming)
;;; init-programming.el ends here
