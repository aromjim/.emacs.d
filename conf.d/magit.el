;;; Configuration of Magit, an interface to the version control system Git

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (setq vc-handled-backends nil))	; Disable Emacs default version control
					; interface
