;;; Configuration of Magit, an interface to the version control system Git

(use-package magit
  :straight t
  :bind ("C-c g" . magit-status)
  :config
  ;; Disable Emacs default version control interface
  (setq vc-handled-backends nil)
  ;; Use the entire frame when displaying a status buffer
  (setq magit-display-buffer-function
	#'magit-display-buffer-fullframe-status-v1)
  ;; Set the commit author for specified repositories
  (add-to-list 'safe-local-variable-values
	       '(magit-commit-arguments .
		("--author=Álvaro Romero-Jiménez <aromjim@gmail.com>")))

  (dir-locals-set-class-variables 'tramp-server-repository
     '((nil . ((magit-commit-arguments .
		("--author=Álvaro Romero-Jiménez <aromjim@gmail.com>"))))))

  (dir-locals-set-directory-class
   "/sudo:root@post.cs.us.es:/etc/" 'tramp-server-repository)

  (dir-locals-set-directory-class
   "/sudo:root@sierpes.cs.us.es:/etc/" 'tramp-server-repository))

  (dir-locals-set-directory-class
   "/sudo:root@maimonides8.changeip.org:/etc/" 'tramp-server-repository))
