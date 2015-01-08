;;; init-org.el --- Initialization and customizations for org mode
;;; Commentary:
;;; Code:

(require 'f)
(require 'org)
(require 'org-agenda)

(defvar tlh/org-dir "~/notes" "Root directory of all org files.")
(defvar tlh/org-to-consume-file (f-join tlh/org-dir "topics" "to-consume.org")
  "Path to the org file containing this to watch, read, and listen to.")

(defun tlh/load-create-current-gtd ()
  "Load the current gtd file based off of date.  Create a new one if neccessary."
  (setf org-default-notes-file (f-join tlh/org-dir "dates" (downcase (format-time-string "%Y-%B.org"))))
  (unless (f-exists? org-default-notes-file)
    (tlh/create-gtd org-default-notes-file)))

(defun tlh/create-gtd (gtd-file)
  "Create a new GTD-FILE."
  (let ((title (format-time-string "%B - %Y")))
    (f-write-text (mapconcat #'identity
                             (list title
                                   ""
                                   "* Tasks"
                                   "#+CATEGORY: Tasks"
                                   ""
                                   "* Misc"
                                   "#+CATEGORY: Misc"
                                   ""
                                   "* Finance"
                                   "#+CATEGORY: Finance"
                                   ""
                                   "* Anniversaries and Holidays"
                                   "#+CATEGORY: Holiday"
                                   "%%(org-calendar-holiday)"
                                   "#+CATEGORY: Birthdays"
                                   )
                             "\n")
                  'utf-8 gtd-file)))

(tlh/load-create-current-gtd)

(setf org-agenda-files (append (f-files (f-join tlh/org-dir "dates")
                                        #'(lambda (file)
                                            (and (not (string= (substring file -1 nil) "~"))
                                                 (not (string= (substring file -1 nil) "#"))
                                                 (string= (f-ext file) "org"))))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?" :prepend t)
        ("m" "Misc" entry (file+headline org-default-notes-file "Misc")
         "* %?")
        ("r" "Read" item (file+headline org-default-notes-file "Read")
         "- %?")
        ("b" "Book" item (file+headline tlh/org-to-consume-file "Books")
         "- %?")
        ("l" "Links" item (file+headline tlh/org-to-consume-file "Links")
         "- %?")
        ("p" "Podcast" item (file+headline tlh/org-to-consume-file "Podcasts")
         "- %?")))

;; keybindings
(define-key global-map (kbd "M-n") 'org-capture)
(define-key global-map (kbd "M-N") #'(lambda () (interactive) (find-file org-default-notes-file)))

(provide 'init-org)
;;; init-org.el ends here
