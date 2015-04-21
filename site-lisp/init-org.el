;;; init-org.el --- Initialization and customizations for org mode
;;; Commentary:
;;; Code:

(require 'f)
(require 'org)
(require 'org-agenda)
(require 'org-capture)

(defvar tlh/org-dir "~/notes" "Root directory of all org files.")
(defvar tlh/org-to-consume-file (f-join tlh/org-dir "topics" "to-consume.org")
  "Path to the org file containing this to watch, read, and listen to.")

(defun tlh/load-create-current-gtd ()
  "Load the current gtd file based off of date.  Create a new one if neccessary."
  (setf org-default-notes-file (f-join tlh/org-dir "dates"
                                       (downcase (format-time-string "%Y-%B.org"))))
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
                                   "* Events"
                                   "#+CATEGORY: Events"
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

(defun tlh/project-name-by-directory (directory)
  "Return the name of the current projects based on the current DIRECTORY."
  (interactive (list default-directory))
  (if (f-ancestor-of? "~/projects" directory)
      (car (f-split (f-relative directory  "~/projects")))
    (error "Not in project directory!")))

(defun tlh/project-org-file-by-directory (directory)
  "Return the org file assocaiated with the current project based on DIRECTORY."
  (let* ((name (tlh/project-name-by-directory directory))
        (org-file (f-join tlh/org-dir "projects" (concat name ".org"))))
    (when (not (f-exists? org-file))
      (f-write-text (mapconcat #'identity
                               (list (concat "#+TITLE: " name)
                                     "#+AUTHOR: Tom Hartman"
                                     ""
                                     "* Todo"
                                     "#+CATEGORY: Todo"
                                     "* Bugs"
                                     "#+CATEGORY: Bugs"
                                     ""
                                     "#+TODO: TODO | DONE"
                                     "#+TODO: FEATURE | IMPLEMENTED"
                                     "#+TODO: REPORT BUG KNOWN | FIXED"
                                     "#+TODO: | CANCELED")
                               "\n")
                    'utf-8 org-file))
    org-file))

(tlh/load-create-current-gtd)

(setf org-agenda-files
      (append (f-files (f-join tlh/org-dir "dates")
                       #'(lambda (file)
                           (and (not (string= (substring file -1 nil) "~"))
                                (not (string= (substring file -1 nil) "#"))
                                (string= (f-ext file) "org"))))))

; Monthly Tasks/Events/Misc
(add-to-list 'org-capture-templates
             '("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
               "* TODO %?" :prepend t))
(add-to-list 'org-capture-templates
             '("e" "Event" entry (file+headline org-default-notes-file "Events")
               "* SCHEDULED %?" :prepend t))
(add-to-list 'org-capture-templates
             '("m" "Misc" entry (file+headline org-default-notes-file "Misc")
               "* %?"))

; Consumables
(add-to-list 'org-capture-templates
             '("r" "Read" item (file+headline tlh/org-to-consume-file "Read")
               "- %?"))
(add-to-list 'org-capture-templates
             '("b" "Book" item (file+headline tlh/org-to-consume-file "Books")
               "- %?"))
(add-to-list 'org-capture-templates
             '("l" "Links" item (file+headline tlh/org-to-consume-file "Links")
              "- %?"))
(add-to-list 'org-capture-templates
             '("p" "Podcast" item (file+headline tlh/org-to-consume-file "Podcasts")
               "- %?"))

;; keybindings
(define-key global-map (kbd "M-n") 'org-capture)
(define-key global-map (kbd "M-N")
  #'(lambda () (interactive)
      (find-file org-default-notes-file)))
(define-key global-map (kbd "M-p") 'tlh/capture-project-entry)
(define-key global-map (kbd "M-P")
  #'(lambda () (interactive)
      (find-file (tlh/project-org-file-by-directory default-directory))))

(defun tlh/capture-project-entry ()
  "Interactive function to capture project specific entries."
  (interactive)
  (let ((project-org-file (tlh/project-org-file-by-directory default-directory))
        (org-capture-templates '(("t" "Todo" entry (file+headline project-org-file "Todo")
                                  "* TODO: %?" :prepend t)
                                 ("f" "Feature" entry (file+headline default-directory "Todo")
                                  "* FEATURE: %?" :prepend t)
                                 ("b" "Bug" entry (file+headline project-org-file "Bugs")
                                  "* BUG: %?" :prepend t))))
    (org-capture)))


;; Test tlh/project-name-by-directory
(ert-deftest tlh/test-project-name-by-directory ()
  (should (string= "foo" (tlh/project-name-by-directory "~/projects/foo")))
  (should (string= "foo" (tlh/project-name-by-directory "~/projects/foo/bar")))
  (should (string= "foo" (tlh/project-name-by-directory "~/projects/foo/bar/baz"))))

(provide 'init-org)
;;; init-org.el ends here
