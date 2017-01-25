;;; Package installation configuration

(use-package package
  :bind ("C-c p" . list-packages)
  :config
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  (setq package-user-dir "/usr/local/share/emacs/site-lisp"))
