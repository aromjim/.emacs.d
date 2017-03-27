;; Human-readable format for file sizes
(setq list-directory-verbose-switches "-lh")
(setq dired-listing-switches "-alh")

;; Recursive copies and deletes allowed
;; but move deleted files and directories into trash
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)

;; Use directory shown in dired buffer of next window as default
;; for file operations
(setq dired-dwim-target t)
