(use-package polymode
  :defer t)

(use-package poly-R
  :mode ("\\.Rnw\\'" . poly-noweb+R-mode)
  :config
  (use-package tex-site))
