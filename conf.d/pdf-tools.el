(use-package pdf-tools
  :defer t
  :init
  (pdf-tools-install)
  :config
  ;; Use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
