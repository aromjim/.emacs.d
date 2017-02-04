(use-package ebib
  :bind ("C-c b" . ebib)
  :config
  ;; Directories containing bibliographic databases
  (setq ebib-bib-search-dirs '("~/.texmf/bibtex/bib/personal"))
  ;; Bibliographic databases to load on startup
  (setq ebib-preload-bib-files '("Bibliography.bib"))
  ;; Biblatex format for bibliographic databases
  (setq ebib-bibtex-dialect 'biblatex)
  ;; Customization of the automatic generation of keys
  (setq ebib-autogenerate-keys t
	bibtex-autokey-names-stretch 1
	bibtex-autokey-name-separator "_"
	bibtex-autokey-additional-names "_y_otros"
	bibtex-autokey-year-length 0
	bibtex-autokey-titleword-length 'infty
	bibtex-autokey-year-title-separator ":")
  ;; Customization of hidden fields
  (--each '("hyphenation" "year") (add-to-list 'ebib-hidden-fields it))
  (--each '("chapter" "edition" "isbn" "issn" "language" "pagetotal")
    (delete it ebib-hidden-fields))
  ;; Customization of extra fields
  (let ((fields (assq 'biblatex ebib-extra-fields)))
    (setcdr fields (append '("langid" "langidopts")
			   (cdr fields))))
  )
