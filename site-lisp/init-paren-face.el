;;; init-paren-face.el --- paren-face initialization and customizations
;;; Commentary:
;;; Code:
(require 'paren-face)

(when (display-graphic-p)
  (set-face-foreground 'paren-face "Gray30"))

(provide 'init-paren-face)
;;; init-paren-face.el ends here
