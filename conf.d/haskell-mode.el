(use-package haskell-mode
  :straight t
  :defer t
  :config
  (add-to-list 'haskell-process-args-ghci "-fshow-loaded-modules"))
