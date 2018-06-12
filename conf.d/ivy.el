(use-package ivy
  :straight t
  :config
  ;; Redefine key bindings for page navigation
  (define-key ivy-minibuffer-map (kbd "<next>") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "<prior>") 'ivy-scroll-down-command)
  ;; Enable Ivy completion everywhere
  (ivy-mode 1))
