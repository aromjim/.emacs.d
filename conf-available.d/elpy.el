(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode))

(use-package elpy
  :after python
  :config
  (elpy-enable))
