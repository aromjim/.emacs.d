(use-package counsel
  :config
  ;; Include file at point when finding files
  (setq counsel-find-file-at-point t)
  ;; Global key bindings
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "<menu>") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c m") 'counsel-bookmark)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char))
