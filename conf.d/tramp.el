;; User emacs file to save server data
(defvar server-file "servers.private")

(defun read-server-file (&optional with-port-only)
  (let* ((filename (locate-user-emacs-file server-file))
	 (server-list (f-read-text filename 'utf-8))
	 (servers (--map (s-split "[[:space:]]+" it)
			 (s-split "\n" server-list t))))
    (if with-port-only
	(--filter (= (length it) 3) servers)
      servers)))

(defun knock-remote-server ()
  (interactive)
  (let* ((servers (read-server-file t))
	 (server-names (-select-column 0 servers))
	 (server (completing-read (concat "Server to knock (default "
					  (car server-names)
					  "): ")
				  server-names nil t nil nil server-names))
	 (port (nth 2 (assoc server servers))))
    (call-process "knock" nil nil nil server port)))

(global-set-key (kbd "C-c k") 'knock-remote-server)

(use-package tramp
  :bind ("C-c tc" . tramp-cleanup-all-buffers)
  :config
  (let ((servers (read-server-file)))
    (--each servers
      (let ((server (car it))
	    (username (cadr it)))
	(add-to-list 'tramp-default-proxies-alist
		     (list server
			   "root"
			   (s-lex-format "/ssh:${username}@${server}:")))))))
