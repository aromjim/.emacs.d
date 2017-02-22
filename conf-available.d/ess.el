(use-package ess-site
  :defer t
  :config
  (delete '("\\.[rR]nw\\'" . Rnw-mode)
	  auto-mode-alist))
