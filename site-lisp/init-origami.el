;;; init-origami.el --- origami initialization and key bindings
;;; Commentary:
;;; Code:

(require 'origami)

;; Origami open
(global-set-key (kbd "C-c o") 'origami-open-node)
(global-set-key (kbd "C-c O") 'origami-open-all-nodes)

;; Origami close
(global-set-key (kbd "C-c c") 'origami-close-node)
(global-set-key (kbd "C-c C") 'origami-close-all-nodes)

;; Origami toggle
(global-set-key (kbd "C-c t") 'origami-toggle-node)
(global-set-key (kbd "C-c T") 'origami-toggle-all-nodes)

(global-origami-mode)

(provide 'init-origami)
;;; init-origami.el ends here
