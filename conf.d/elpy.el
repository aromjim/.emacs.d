(use-package python
  :mode ("\\.py\\'" . python-mode))

(use-package elpy
  :after python
  :config
  (setq python-shell-interpreter "python3")
  (elpy-enable))

(use-package pyvenv
  :bind ("C-c e" . pyvenv-activate)
  :config
  (pyvenv-mode 1))
