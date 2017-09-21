(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("<menu>" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x l" . counsel-locate)
	 ("C-c m" . counsel-bookmark)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char))
  :config
  ;; Include file at point when finding files
  (setq counsel-find-file-at-point t))
