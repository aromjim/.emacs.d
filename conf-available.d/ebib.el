(use-package ebib
  :bind ("C-c b" . ebib)
  :config
  ;; Directories containing bibliographic databases
  (setq ebib-bib-search-dirs '("~/.texmf/bibtex/bib/personal"))
  ;; Bibliographic databases to load on startup
  (setq ebib-preload-bib-files '("Bibliography.bib"))
  ;; Don't create backup files for bibliographic databases
  (setq ebib-create-backups nil)
  ;; Biblatex format for bibliographic databases
  (setq ebib-bibtex-dialect 'biblatex)
  ;; Customization of the table format for index buffer
  (setq ebib-index-columns
	(-replace '("Year" 6 t) '("Date" 6 t) ebib-index-columns))
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
  ;; Directories for the external files of the bibliographic items
  ;; The first one is Biblioteca so that asking for files is done from it
  (setq ebib-file-search-dirs '("~/Biblioteca"
				"~/Biblioteca/Actas de congresos"
				"~/Biblioteca/Artículos en revistas"
				"~/Biblioteca/Contribuciones a congresos"
				"~/Biblioteca/Libros"
				"~/Biblioteca/Tesis doctorales"))
  ;; Associations of file extensions with external viewers
  (setq ebib-file-associations '(("pdf" . "evince")
				 ("ps" . "evince")))
  ;; File where keywords from all bibliographic databases are saved
  (setq ebib-keywords-file "~/.texmf/bibtex/bib/ebib-keywords.txt")
  ;; Keep keyword file sorted and without duplicates
  (setq ebib-keywords-field-keep-sorted t)
  ;; Major mode for multiline edit buffer
  (setq ebib-multiline-major-mode 'markdown-mode)
  )
