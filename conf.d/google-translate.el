(use-package google-translate
  :defer t)

(use-package google-translate-smooth-ui
  :straight nil
  :bind ("C-c t" . google-translate-smooth-translate)
  :config
  (setq google-translate-translation-directions-alist
	'(("es" . "en") ("en" . "es")))
  (setq google-translate-listen-program "cvlc"))
