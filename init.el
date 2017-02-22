(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(load-dirs "~/.emacs.d/conf.d")
 '(package-selected-packages
   (quote
    (ess elpy ivy-hydra ivy markdown-mode ebib bookmark+ magit f s auctex use-package load-dir))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Package installation configuration
(require 'package)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

(setq package-user-dir "/usr/local/share/emacs/site-lisp")

(package-initialize)

(global-set-key (kbd "C-c p") 'list-packages)

;; Package configurations manage by use-package
(require 'use-package)

;; Load all configuration files in ~/.emacs.d/conf.d
(require 'load-dir)
