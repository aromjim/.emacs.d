(use-package ebib
  :bind ("C-c b" . ebib)
  :config
  ;; Directories containing bibliographic databases
  (setq ebib-bib-search-dirs '("~/.texmf/bibtex/bib/personal"))
  ;; Bibliographic databases to load on startup
  (setq ebib-preload-bib-files '("Bibliography.bib"))
  )
