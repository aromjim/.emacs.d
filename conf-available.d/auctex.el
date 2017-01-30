;;; Configuration of AUCTeX, a package for writing (La)TeX files

(use-package tex
  :defer t
  :config
  ;; Activate on the fly spell checking
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

  ;; Activate RefTeX mode
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)

  ;; Bound character '\' to TeX-electric-macro
  (setq TeX-electric-escape t)

  ;; Automatically insert the opening and closing symbols for an inline equation
  (setq TeX-electric-math t)

  ;; Automatically insert braces for sub and superscripts in math mode
  (setq TeX-electric-sub-and-superscript t)

  ;; Automatically insert the corresponding closing brace when writing an opening brace
  (setq LaTeX-electric-left-right-brace t))
