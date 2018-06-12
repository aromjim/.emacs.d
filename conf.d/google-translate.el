(use-package google-translate
  :straight t
  :defer t)

(use-package google-translate-smooth-ui
  :bind ("C-c t" . google-translate-smooth-translate)
  :config
  (setq google-translate-translation-directions-alist
	'(("es" . "en") ("en" . "es")))
  (setq google-translate-listen-program "cvlc"))
