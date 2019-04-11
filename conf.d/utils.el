(use-package cl-lib
  :commands cl-flet			; Used in AUCTeX configuration
  )

(use-package dash
  :commands (--each			; Used in AUCTeX configuration
    					; Used in Ebib configuration
    					; Used in TRAMP configuration
    					; Used in RefTeX configuration
	     --filter			; Used in TRAMP configuration
	     --map			; Used in insert-date command
	     )
  )

(use-package f
  :commands f-read-text			; Used in TRAMP configuration
  )

(use-package s
  :commands s-lex-format		; Used in TRAMP configuration
  )
