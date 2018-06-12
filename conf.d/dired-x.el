(use-package dired-x
  :config
  ;; Alist of file regexps and their suggested commands
  (setq dired-guess-shell-alist-user
	'(("^.*\\.odt$" "soffice")
	  ("^.*\\.ps$" "evince")
	  ("^.*\\.pdf$" "evince"))))
