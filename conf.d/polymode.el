(use-package polymode
  :straight t
  :defer t)

(use-package poly-R
  :straight t
  :mode ("\\.Rnw\\'" . poly-noweb+R-mode)
  :config
  (use-package tex-site))
