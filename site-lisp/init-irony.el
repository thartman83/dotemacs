;;; init-irony.el --- Irony configuration and initialization
;;; Commentary:
;;; Code:
(require 'irony)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(defun tlh/irony-mode-hook ()
  "Mode hook to be called to setup irony."
  (define-key irony-mode-map [remap completeion-at-point]
    'irony-completion-at-point)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'tlh/irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(provide 'init-irony)
;;; init-irony.el ends here
