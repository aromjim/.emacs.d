;;; Configuration of AUCTeX, a package for writing (La)TeX files

(use-package tex
  :defer t
  :config
  ;; Activate on the fly spell checking
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

  ;; Activate RefTeX mode
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))
