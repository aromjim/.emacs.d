(use-package bookmark+
  :straight t
  :config
  ;; Don't automatically save bookmark list state
  (setq bmkp-bmenu-state-file nil)
  ;; Don't show filenames in bookmark list
  (setq bookmark-bmenu-toggle-filenames nil)
  ;; Sort tagged before untagged bookmarks
  (setq bmkp-sort-comparer '((bmkp-tagged-cp) bmkp-alpha-p))
  ;; Refresh bookmark list when a bookmark is set
  (bmkp-toggle-bookmark-set-refreshes)
  ;; Create and display bookmark list at startup
  (setq initial-buffer-choice (lambda ()
				(bookmark-bmenu-list)
				(bmkp-bmenu-refresh-menu-list)
				(get-buffer "*Bookmark List*"))))
