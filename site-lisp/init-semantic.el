;;; init-semantic.el --- Customizations and initializations for semantic
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

(provide 'init-semantic)
;;; init-semantic.el ends here
