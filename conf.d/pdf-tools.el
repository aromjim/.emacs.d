(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  ;; Use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
