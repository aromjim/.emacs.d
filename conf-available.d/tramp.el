(use-package tramp
  :defer t
  :config
  (use-package f)
  (use-package s)

  ;; User emacs file to save server data
  (defvar servers-file "servers.private")
  
  (let* ((add-server (lambda (server username) ; Add hop for sudoing into server
		       (add-to-list 'tramp-default-proxies-alist
				    (list server
					  "root"
					  (s-lex-format "/ssh:${username}@${server}:")))))
	 (filename (locate-user-emacs-file servers-file))
	 (servers (f-read-text filename 'utf-8)))
    (mapc (lambda (server)
	    (apply add-server (s-split "[[:space:]]+" server)))
	  (s-split "\n" servers t))))
