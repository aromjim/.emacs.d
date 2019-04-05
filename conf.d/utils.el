(use-package cl-lib
  :commands cl-flet			; Used in AUCTeX configuration
  )

(use-package dash
  :straight t
  :commands (--each			; Used in AUCTeX configuration
    					; Used in Ebib configuration
    					; Used in TRAMP configuration
    					; Used in RefTeX configuration
	     --filter			; Used in TRAMP configuration
	     --map			; Used in insert-date command
	     )
  )

(use-package f
  :straight t
  :commands f-read-text			; Used in TRAMP configuration
  )

(use-package s
  :straight t
  :commands s-lex-format		; Used in TRAMP configuration
  )

;; Command for inserting current date
(defvar date-formats
  '("%0A %-e de %B de %Y"		; viernes 5 de abril de 2019
    "%-e de %B de %Y"			; 5 de abril de 2019
    "%d-%m-%Y"				; 05-04-2019
    "%d/%m/%Y"				; 05/04/2019
    "%F"				; 2019-04-05
    "%Y/%m/%d"				; 2019/04/05
    )
  "Available date formats for insert-date command")

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (ivy-read "Insert date: "
	    (--map (format-time-string it) date-formats)
	    :preselect 0
	    :action (lambda (date)
		      (when (use-region-p) (delete-region (region-beginning) (region-end)))
		      (insert date))
	    :caller 'insert-date))

(global-set-key (kbd "C-c d") 'insert-date)
