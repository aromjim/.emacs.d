;;; Configuration of Magit, an interface to the version control system Git

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  ;; Disable Emacs default version control interface
  (setq vc-handled-backends nil))
