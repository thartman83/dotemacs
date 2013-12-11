;;; test-string-utils.el --- Testing functions for string-utils.el

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

;; 

;;; Code:

(require 'string-utils)
(require 'ht)

(ert-deftest test-format-hash ()
  ; identity tests
  (should (string= (format-hash "foobar" (ht-create)) "foobar"))
  (should (string= (format-hash ":foobar" (ht (:foobar "bazblah")))
                   ":foobar"))
  ; basic replacement
  (should (string= (format-hash "foo%bar%baz" (ht (:bar "bal")))
                   "foobalbaz"))
  (should (string= (format-hash "%foo%bar" (ht (:foo "baz")))
                   "bazbar"))
  (should (string= (format-hash "foo%bar%" (ht (:bar "baz")))
                   "foobaz"))
  ; escape control character
  (should (string= (format-hash "foo%%bar" (ht-create)) "foo%bar"))
  (should (string= (format-hash "%%foo%%bar%%" (ht-create)) "%foo%bar%"))
  ; the whole shebang
  (should (string= (format-hash "%%%foo%%%%bar%%%" (ht (:foo "baz") (:bar "blah")))
                   "%baz%blah%")))

(ert-deftest test-find-chars ()
  (should (equal (find-chars "foobar" ?z) nil))
  (should (equal (find-chars "foobar" ?o) '(1 2)))
  (should (equal (find-chars "bobob" ?b) '(0 2 4)))
  (should (equal (find-chars "oooooo" ?o 10) nil))
  (should (equal (find-chars "oooooo" ?o 2) '(2 3 4 5)))
  (should (equal (find-chars "oooooo" ?o 0 2) '(0 1)))
  (should (equal (find-chars "oooooo" ?o 3 2) '(3 4))))

(provide 'test-string-utils)

;;; test-string-utils.el ends here
