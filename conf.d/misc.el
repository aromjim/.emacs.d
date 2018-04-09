;;; Emacs look

;; Window title
(setq frame-title-format '("%*" "%b" " en " system-name))

;; Default font
(set-fontset-font t 'unicode "Ubuntu Mono")
(set-face-attribute 'default nil :height 160)
(set-face-font 'default "fontset-default")

;; Disable toolbar
(tool-bar-mode -1)

;; Create frames maximized
(add-to-list 'default-frame-alist
	     '(fullscreen . maximized))


;;; Configuration of CUA mode

;; Activate CUA mode
(cua-mode t)

;; Time to delay before overriding prefix key 
(setq cua-prefix-override-inhibit-delay 0.5)

;; Activate Delete Selection mode
(delete-selection-mode)


;;; Miscellaneous configurations

;; Column number in mode line
(column-number-mode t)

;; Default value for automatic line-wrapping column
(setq-default fill-column 79)

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
