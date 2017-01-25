;;; Emacs look

;; Window title
(setq frame-title-format '("%*" "%b" " en " system-name))

;; Initial window position and size
(add-to-list 'initial-frame-alist '(top . 0))
(add-to-list 'initial-frame-alist '(left . 0))
(add-to-list 'initial-frame-alist '(height . 47))
(add-to-list 'initial-frame-alist '(width . 120))

;; Default font
(set-fontset-font t 'unicode "Ubuntu Mono")
(set-face-attribute 'default nil :height 160)
(set-face-font 'default "fontset-default")

;; Disable toolbar
(tool-bar-mode -1)
