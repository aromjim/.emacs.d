(use-package cl-lib
  :commands cl-flet			; Used in AUCTeX configuration
  )

(use-package dash
  :straight t
  :commands (--each			; Used in AUCTeX configuration
    					; Used in Ebib configuration
    					; Used in TRAMP configuration
	     --filter)			; Used in TRAMP configuration
  )

(use-package f
  :straight t
  :commands f-read-text			; Used in TRAMP configuration
  )

(use-package s
  :straight t
  :commands s-lex-format		; Used in TRAMP configuration
  )
