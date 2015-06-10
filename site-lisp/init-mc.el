;;; init-mc.el --- Initialize mc mode and functions

;; Copyright (c) 2013 Thomas Hartman (rokstar83@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;;; Code:
(require 'multiple-cursors)

(define-key global-map (kbd "C-c l")
  #'mc/edit-lines)

(define-key global-map (kbd "C-c m")
  #'mc/mark-all-dwim)

(provide 'init-mc)
;;; init-mc.el ends here
