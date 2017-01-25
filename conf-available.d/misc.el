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


;;; Miscellaneous configurations

;; Column number in mode line
(column-number-mode t)

;; Default value for automatic line-wrapping column
(setq-default fill-column 99)

;; Visualize matching parens
(show-paren-mode t)

;; Avoid cursor and mouse pointer clash
(mouse-avoidance-mode 'animate)

;; Save minibuffer history
(savehist-mode)

;; Activate Auto Fill in Text mode and related modes
(toggle-text-mode-auto-fill)

;; A single space does end a sentence
(setq sentence-end-double-space nil)

;; Don't make a backup of a file the first time it is saved
(setq make-backup-files nil)
