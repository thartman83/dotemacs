;;; init-auto-complete.el --- Initialize auto complete
;;; Commentary:
;;; Code:
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-c-headers)

(ac-config-default)

(defun tlh/ac-c-header-init ()
  "Initialize ac-c-header hook."
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (require 'auto-complete-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-headers)))

(add-hook 'c-mode-hook
          #'(lambda ()
              (require 'auto-complete-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-headers)))

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
