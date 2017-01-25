(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(load-dirs "~/.emacs.d/conf.d")
 '(package-selected-packages (quote (auctex magit use-package load-dir))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Package installation configuration
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(setq package-user-dir "/usr/local/share/emacs/site-lisp")

(package-initialize)

(global-set-key (kbd "C-c p") 'list-packages)

;; Package configurations manage by use-package
(require 'use-package)

;; Load all configuration files in ~/.emacs.d/conf.d
(require 'load-dir)
