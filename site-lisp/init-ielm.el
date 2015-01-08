;;; init-ielm.el --- Initializations and customizations for ielm
;;; Commentary:
;;; Code:
(require 'ielm)

(defadvice ielm-eval-input (after ielm-paredit activate)
  "Begin each ielm prompt with a paredit pair." (paredit-open-round))

(provide 'init-ielm)
;;; init-ielm.el ends here
