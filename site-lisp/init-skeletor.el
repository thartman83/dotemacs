;;; init-skeletor.el -- skeletor initialization
;;; Commentary:
;;;
;;; Code:

(require 'skeletor)

(skeletor-define-template "cpp-project"
  :default-license ("gpl")
  :after-creation (lambda (dir)
                    (skeletor-async-shell-command "git init")
                    (message "Setting up Autotools... ")
                    (skeletor-async-shell-command "autoreconf -i && ./configure")
                    (message "Autotools Setup Complete")))

(provide 'init-skeletor)

;;; init-skeletor ends here
