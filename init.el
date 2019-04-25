;; Use develop version of straight.el
(setq straight-repository-branch "develop")

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
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
(setq straight-use-package-by-default t)
(setq use-package-always-demand t)

;; Load all configuration files in ~/.emacs.d/conf.d
(use-package load-dir
  :init
  (setq load-dirs "~/.emacs.d/conf.d"))
