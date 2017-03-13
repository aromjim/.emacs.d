(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode))

(use-package elpy
  :after python
  :config
  (setq python-shell-interpreter "python3")
  (elpy-enable))
