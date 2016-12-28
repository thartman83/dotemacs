;;; init-openscad.el --- Intialize OpenSCAD mode

;; Copyright (c) 2016 Thomas Hartman (rokstar83@gmail.com)

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

;; 

;;; Code:

(require 'scad-mode)
(require 'scad-preview)

(custom-set-variables '(scad-preview-image-size '(900 . 900)))
(add-to-list 'auto-mode-alist '("\\.scad\\'" . scad-mode))

(defun scad-export-stl ()
  "Exports the current visited filename as an stl file."
  (interactive)
  (call-process "openscad" nil "*openscad-output*" t
                "-o" (f-swap-ext (f-filename (buffer-file-name)) "stl")
                (buffer-file-name)))

(provide 'init-openscad)

;;; init-openscad.el ends here
