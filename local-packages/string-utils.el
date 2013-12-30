;;; string-utils.el --- 

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

;; string-utils --- string utilities

(require 'ht)

(defun comment-lines (str begin-comment &optional end-comment line-len)
  "Return STR with BEGIN-COMMENT starting each line.  Optionally append END-COMMENT
to the end of each line as well.  If LINE-LEN is specified then END-COMMENT is appended
with the number of spaces neccessary to force the END-COMMENT to the end of the line."
  (let ((lines (split-string str "\n")))
    (mapconcat 'identity
               (mapcar #'(lambda (line)
                           (if (null end-comment)
                               (format "%s%s" begin-comment line)
                             (if (null line-len)
                                 (format "%s%s%s" begin-comment line end-comment)
                               (format (format "%%s%%-%ds%%s" (- line-len (+ (length end-comment)
                                                                             (length begin-comment))))
                                       begin-comment line end-comment))))
                       lines)
               "\n")))

(defun find-chars (str c &optional from count)
  "Return the positions within STR of each character C,
Optionally start at index FROM and only return COUNT number of indicies."
  (when (null from)
    (setq from 0))
  (when (null count)
    (setq count (length str)))
  (if (or (= 0 count) (> from (1- (length str))))
      nil
    (if (= (elt str from) c)
        (cons from (find-chars str c (1+ from) (1- count)))
      (find-chars str c (1+ from) count))))

(defun replace-substring (str new-str from to)
  "Replace the substring within STR with NEW-STR between [FROM...TO]."
  (concat (if (= from 0) "" (substring str 0 from))
          new-str
          (if (= from (1- (length str))) "" (substring str to))))

(defun substringify (str indicies)
  "Split STR in substrings based on INDICIES.

Indicies is a list of index pairs of substrings."
  (if (null indicies)
      nil
    (cons (substring str (first indicies) (1+ (second indicies)))
          (substringify str (rest (rest indicies))))))

(defun format-hash (str hash)
  "Replace control sequences in STR with their values within HASH.

Control sequences are surrounded by % and represent keys within HASH.
For the moment this function assumes that keys with in the HASH are keywords
and the control sequences lack the colon keyword prefix."
  (let ((indicies (find-chars str ?% 0 2))
        (cur-pos 0))
    (while (not (null indicies))
      (cond
       ((not (= (length indicies) 2))
        (error "improperly formatted control string"))
       ((= (1+ (first indicies)) (second indicies))
        (setq str (replace-substring str "%" (first indicies) (1+ (second indicies))))
        (setq cur-pos (second indicies)))
       (t 
        (let ((key (intern (concat ":" (substring str (1+ (first indicies)) 
                                                  (second indicies))))))
          (when (not (ht-contains-p hash key))
            (error "'%s' is not a key in provided hash table" key)) 
          (setq str (replace-substring str (ht-get hash key) (first indicies) 
                                       (1+ (second indicies))))
          (setq cur-pos (+ (first indicies) (length (ht-get hash key)))))))
      (setq indicies (find-chars str ?% cur-pos 2)))
    str))

(defun fill-in-gaps (indicies len &optional start)
  "Fill in the substring gaps in INDICIES for string LEN size at position START."
  (cond 
   ((null start)
    (fill-in-gaps indicies len 0))
   ((null indicies) (list start (1- len)))
   (t (let* ((next-pair (if (= start (first indicies))
                            (list (first indicies) (second indicies))
                          (list start (1- (first indicies)))))
             (next-list (if (= start (first indicies))
                           (rest (rest indicies))
                         indicies)))
        (append next-pair (fill-in-gaps next-list len (1+ (second next-pair))))))))

(defun format-hash-functional (str hash)
  "Replace control sequences in STR with their values within HASH.

Control sequences are surrounded by % and represent keys within HASH.
For the moment this function assumes that keys with in the HASH are keywords
and the control sequences lack the colon keyword prefix."  
  (let ((indicies (find-chars str ?%)))
    (if (null indicies)
        str
      (mapconcat #'(lambda (part)
                     (cond
                      ((string= part "") "")
                      ((string= part "%%") "%")
                      ((and (= (elt part 0) ?%) (= (elt part (1- (length part))) ?%))
                       (let ((key (intern (concat ":" (substring part 1 (1- (length part)))))))
                         (when (not (ht-contains-p hash key))
                           (error "'%s' is not a key in provided hash table" key))
                         (ht-get hash key)))                      
                      (t part)))
                 (substringify str (fill-in-gaps indicies (length str)))
                 ""))))

(provide 'string-utils)

;;; string-utils.el ends here
