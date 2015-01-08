;;; init-flycheck.el --- Initialization and customization of flycheck
;;; Commentary:
;;; Code:
(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
  ;; clang option to set what c/c++ standard to use
(flycheck-def-option-var flycheck-clang-std "" c/c++-clang
    "Which standard of c/c++ to use when invoking clang.

The value of this variable is a string, which is appended to -std=. Options are
c89, gnu89, c94, c99, gnu99, c++98 and c++11. Default value is an empty string
and uses clangs default c or c++ standard (gnu99 or c++98)."
    :type '(choice (const :tag "Clang default" "")
             (const :tag "c89 standard" "c89")
             (const :tag "gnu89 standard" "gnu89")
             (const :tag "c94 standard" "c94")
             (const :tag "c99 standard" "c99")
             (const :tag "gnu99 standard" "gnu99")
             (const :tag "c++98 standard" "c++98")
             (const :tag "c++11 standard" "c++11"))
    :safe #'stringp
    :package-version '(flycheck . "0.14"))
  ;; slightly modified version of c/c++-clang to allow for change which c/c++ standard
  ;; to use as well as always including the local directory as an include path
  (flycheck-define-checker c/c++-clang
    "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
    :command ("clang"
        "-fsyntax-only"
        "-fno-color-diagnostics"    ; Do not include color codes in output
        "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
        "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
        (option-list "-W" flycheck-clang-warnings s-prepend)
        (option-list "-I" flycheck-clang-include-path)
        (eval (concat "-I" default-directory))
        ;; (eval (when (not (string= flycheck-clang-std ""))
        ;;        (concat "-std=" flycheck-clang-std)))
        "-x" (eval
              (cl-case major-mode
                (c++-mode "c++")
                (c-mode "c"))) source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column
        ": warning: " (message) line-end)
     (error line-start (file-name) ":" line ":" column
      ": " (or "fatal error" "error") ": " (message) line-end))
    :modes (c-mode c++-mode)
    :next-checkers ((warnings-only . c/c++-cppcheck)))

;; always default to the new c++ standard for parsing
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-clang-std "c++11"))

;; This is to avoid load-path issues for local .el work
(setq-default flycheck-emacs-lisp-load-path load-path)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
