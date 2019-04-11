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

(defun insert-date (from-calendar)
  "Insert date at point."
  (interactive "P")
  (let (date)
    (when from-calendar
      (setq date (org-read-date nil t)))
    (ivy-read "Insert date: "
	      (--map (format-time-string it date) date-formats)
	      :preselect 0
	      :action (lambda (date)
			(when (use-region-p)
			  (delete-region (region-beginning) (region-end)))
			(insert date))
	      :caller 'insert-date)))

(global-set-key (kbd "C-c d") 'insert-date)
