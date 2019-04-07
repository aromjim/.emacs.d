(use-package haskell-mode
  :defer t
  :config
  (add-to-list 'haskell-process-args-ghci "-fshow-loaded-modules"))
