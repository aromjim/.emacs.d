;; Bootstrap straight.el
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Require use-package
(straight-use-package 'use-package)
(require 'use-package)

;; Load all configuration files in ~/.emacs.d/conf.d
(use-package load-dir
  :straight t
  :init
  (setq load-dirs "~/.emacs.d/conf.d"))
