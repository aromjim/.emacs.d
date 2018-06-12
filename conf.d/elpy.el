(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode))

(use-package elpy
  :straight t
  :after python
  :config
  (setq python-shell-interpreter "python3")
  (elpy-enable))

(use-package pyvenv
  :straight t
  :demand
  :bind ("C-c e" . pyvenv-activate)
  :config
  (pyvenv-mode 1))
