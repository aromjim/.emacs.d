(use-package tex-site
  :straight (auctex :type git
		    :host github
		    :repo "emacs-straight/auctex"
		    :files ("*.el"
  			    ("images/" "images/*.xpm")
  			    ("style/" "style/*.el" "style/.nosearch")))
  :mode
  ("\\.tex\\'" . TeX-latex-mode)
  :config
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)

  ;; Always start the server for inverse correlate search
  (setq TeX-source-correlate-start-server t)

  ;; Parse LaTeX documents after loading
  (setq TeX-parse-self t)

  ;; Bound character '\' to TeX-electric-macro
  (setq TeX-electric-escape t)

  ;; Don't insert empty braces for commands without arguments
  (setq TeX-insert-braces nil)

  ;; Automatically insert the opening and closing symbols for an inline
  ;; equation
  (setq TeX-electric-math '("$" . "$"))

  ;; Automatically insert braces for sub and superscripts in math mode
  (setq TeX-electric-sub-and-superscript t)

  ;; Automatically insert the corresponding closing brace when writing an
  ;; opening brace
  (setq LaTeX-electric-left-right-brace t))

(use-package latex
  :straight nil
  :after tex-site
  :hook ((LaTeX-mode . turn-on-flyspell) ; Activate on the fly spell checking
	 (LaTeX-mode . TeX-source-correlate-mode) ; Activate forward and
						  ; inverse search to and from
						  ; the viewer
	 )
  :config
  ;; Intermediate files of xsim package
  (add-to-list 'LaTeX-clean-intermediate-suffixes ".*-body\\.tex")

  ;; Keybindings to introduce unicode characters
  (cl-flet ((add-unicode-keymap (keys &optional prefix)
	      (setq prefix (or prefix ""))
	      (--each keys
		(let* ((key (car it))
		       (unicode-name (cdr it))
		       (unicode-code (gethash unicode-name (ucs-names))))
		  (define-key LaTeX-mode-map
		    (vconcat "`" prefix key)
		    `(lambda ()
		       (interactive)
		       (insert-char ,unicode-code)))))))
    (let ((greek-letters
	   '(("A" . "GREEK CAPITAL LETTER ALPHA")
	     ("B" . "GREEK CAPITAL LETTER BETA")
	     ("G" . "GREEK CAPITAL LETTER GAMMA")
	     ("D" . "GREEK CAPITAL LETTER DELTA")
	     ("E" . "GREEK CAPITAL LETTER EPSILON")
	     ("Z" . "GREEK CAPITAL LETTER ZETA")
	     ("H" . "GREEK CAPITAL LETTER ETA")
	     ("J" . "GREEK CAPITAL LETTER THETA")
	     ("I" . "GREEK CAPITAL LETTER IOTA")
	     ("K" . "GREEK CAPITAL LETTER KAPPA")
	     ("L" . "GREEK CAPITAL LETTER LAMBDA")
	     ("M" . "GREEK CAPITAL LETTER MU")
	     ("N" . "GREEK CAPITAL LETTER NU")
	     ("X" . "GREEK CAPITAL LETTER XI")
	     ("O" . "GREEK CAPITAL LETTER OMICRON")
	     ("P" . "GREEK CAPITAL LETTER PI")
	     ("R" . "GREEK CAPITAL LETTER RHO")
	     ("S" . "GREEK CAPITAL LETTER SIGMA")
	     ("T" . "GREEK CAPITAL LETTER TAU")
	     ("U" . "GREEK CAPITAL LETTER UPSILON")
	     ("F" . "GREEK CAPITAL LETTER PHI")
	     ("Q" . "GREEK CAPITAL LETTER CHI")
	     ("Y" . "GREEK CAPITAL LETTER PSI")
	     ("W" . "GREEK CAPITAL LETTER OMEGA")
	     ("a" . "GREEK SMALL LETTER ALPHA")
	     ("b" . "GREEK SMALL LETTER BETA")
	     ("g" . "GREEK SMALL LETTER GAMMA")
	     ("d" . "GREEK SMALL LETTER DELTA")
	     ("e" . "GREEK SMALL LETTER EPSILON")
	     ("z" . "GREEK SMALL LETTER ZETA")
	     ("h" . "GREEK SMALL LETTER ETA")
	     ("j" . "GREEK SMALL LETTER THETA")
	     ("i" . "GREEK SMALL LETTER IOTA")
	     ("k" . "GREEK SMALL LETTER KAPPA")
	     ("l" . "GREEK SMALL LETTER LAMBDA")
	     ("m" . "GREEK SMALL LETTER MU")
	     ("n" . "GREEK SMALL LETTER NU")
	     ("x" . "GREEK SMALL LETTER XI")
	     ("o" . "GREEK SMALL LETTER OMICRON")
	     ("p" . "GREEK SMALL LETTER PI")
	     ("r" . "GREEK SMALL LETTER RHO")
	     ("s" . "GREEK SMALL LETTER SIGMA")
	     ("t" . "GREEK SMALL LETTER TAU")
	     ("u" . "GREEK SMALL LETTER UPSILON")
	     ("f" . "GREEK SMALL LETTER PHI")
	     ("q" . "GREEK SMALL LETTER CHI")
	     ("y" . "GREEK SMALL LETTER PSI")
	     ("w" . "GREEK SMALL LETTER OMEGA")))
	  (greek-variant-letters
	   '(("J" . "GREEK CAPITAL THETA SYMBOL")
	     ("k" . "GREEK KAPPA SYMBOL")
	     ("f" . "GREEK PHI SYMBOL")
	     ("p" . "GREEK PI SYMBOL")
	     ("r" . "GREEK RHO SYMBOL")))
	  (mathematical-script-letters
	   '(("A" . "MATHEMATICAL SCRIPT CAPITAL A")
	     ("B" . "SCRIPT CAPITAL B")
	     ("C" . "MATHEMATICAL SCRIPT CAPITAL C")
	     ("D" . "MATHEMATICAL SCRIPT CAPITAL D")
	     ("E" . "SCRIPT CAPITAL E")
	     ("F" . "SCRIPT CAPITAL F")
	     ("G" . "MATHEMATICAL SCRIPT CAPITAL G")
	     ("H" . "SCRIPT CAPITAL H")
	     ("I" . "SCRIPT CAPITAL I")
	     ("J" . "MATHEMATICAL SCRIPT CAPITAL J")
	     ("K" . "MATHEMATICAL SCRIPT CAPITAL K")
	     ("L" . "SCRIPT CAPITAL L")
	     ("M" . "SCRIPT CAPITAL M")
	     ("N" . "MATHEMATICAL SCRIPT CAPITAL N")
	     ("O" . "MATHEMATICAL SCRIPT CAPITAL O")
	     ("P" . "MATHEMATICAL SCRIPT CAPITAL P")
	     ("Q" . "MATHEMATICAL SCRIPT CAPITAL Q")
	     ("R" . "SCRIPT CAPITAL R")
	     ("S" . "MATHEMATICAL SCRIPT CAPITAL S")
	     ("T" . "MATHEMATICAL SCRIPT CAPITAL T")
	     ("U" . "MATHEMATICAL SCRIPT CAPITAL U")
	     ("V" . "MATHEMATICAL SCRIPT CAPITAL V")
	     ("W" . "MATHEMATICAL SCRIPT CAPITAL W")
	     ("X" . "MATHEMATICAL SCRIPT CAPITAL X")
	     ("Y" . "MATHEMATICAL SCRIPT CAPITAL Y")
	     ("Z" . "MATHEMATICAL SCRIPT CAPITAL Z")
	     ("a" . "MATHEMATICAL SCRIPT SMALL A")
	     ("b" . "MATHEMATICAL SCRIPT SMALL B")
	     ("c" . "MATHEMATICAL SCRIPT SMALL C")
	     ("d" . "MATHEMATICAL SCRIPT SMALL D")
	     ("e" . "SCRIPT SMALL E")
	     ("f" . "MATHEMATICAL SCRIPT SMALL F")
	     ("g" . "SCRIPT SMALL G")
	     ("h" . "MATHEMATICAL SCRIPT SMALL H")
	     ("i" . "MATHEMATICAL SCRIPT SMALL I")
	     ("j" . "MATHEMATICAL SCRIPT SMALL J")
	     ("k" . "MATHEMATICAL SCRIPT SMALL K")
	     ("l" . "MATHEMATICAL SCRIPT SMALL L")
	     ("m" . "MATHEMATICAL SCRIPT SMALL M")
	     ("n" . "MATHEMATICAL SCRIPT SMALL N")
	     ("o" . "SCRIPT SMALL O")
	     ("p" . "MATHEMATICAL SCRIPT SMALL P")
	     ("q" . "MATHEMATICAL SCRIPT SMALL Q")
	     ("r" . "MATHEMATICAL SCRIPT SMALL R")
	     ("s" . "MATHEMATICAL SCRIPT SMALL S")
	     ("t" . "MATHEMATICAL SCRIPT SMALL T")
	     ("u" . "MATHEMATICAL SCRIPT SMALL U")
	     ("v" . "MATHEMATICAL SCRIPT SMALL V")
	     ("w" . "MATHEMATICAL SCRIPT SMALL W")
	     ("x" . "MATHEMATICAL SCRIPT SMALL X")
	     ("y" . "MATHEMATICAL SCRIPT SMALL Y")
	     ("z" . "MATHEMATICAL SCRIPT SMALL Z")))
	  (mathematical-italic-letters
	   '(("A" . "MATHEMATICAL ITALIC CAPITAL A")
	     ("B" . "MATHEMATICAL ITALIC CAPITAL B")
	     ("C" . "MATHEMATICAL ITALIC CAPITAL C")
	     ("D" . "MATHEMATICAL ITALIC CAPITAL D")
	     ("E" . "MATHEMATICAL ITALIC CAPITAL E")
	     ("F" . "MATHEMATICAL ITALIC CAPITAL F")
	     ("G" . "MATHEMATICAL ITALIC CAPITAL G")
	     ("H" . "MATHEMATICAL ITALIC CAPITAL H")
	     ("I" . "MATHEMATICAL ITALIC CAPITAL I")
	     ("J" . "MATHEMATICAL ITALIC CAPITAL J")
	     ("K" . "MATHEMATICAL ITALIC CAPITAL K")
	     ("L" . "MATHEMATICAL ITALIC CAPITAL L")
	     ("M" . "MATHEMATICAL ITALIC CAPITAL M")
	     ("N" . "MATHEMATICAL ITALIC CAPITAL N")
	     ("O" . "MATHEMATICAL ITALIC CAPITAL O")
	     ("P" . "MATHEMATICAL ITALIC CAPITAL P")
	     ("Q" . "MATHEMATICAL ITALIC CAPITAL Q")
	     ("R" . "MATHEMATICAL ITALIC CAPITAL R")
	     ("S" . "MATHEMATICAL ITALIC CAPITAL S")
	     ("T" . "MATHEMATICAL ITALIC CAPITAL T")
	     ("U" . "MATHEMATICAL ITALIC CAPITAL U")
	     ("V" . "MATHEMATICAL ITALIC CAPITAL V")
	     ("W" . "MATHEMATICAL ITALIC CAPITAL W")
	     ("X" . "MATHEMATICAL ITALIC CAPITAL X")
	     ("Y" . "MATHEMATICAL ITALIC CAPITAL Y")
	     ("Z" . "MATHEMATICAL ITALIC CAPITAL Z")
	     ("a" . "MATHEMATICAL ITALIC SMALL A")
	     ("b" . "MATHEMATICAL ITALIC SMALL B")
	     ("c" . "MATHEMATICAL ITALIC SMALL C")
	     ("d" . "MATHEMATICAL ITALIC SMALL D")
	     ("e" . "MATHEMATICAL ITALIC SMALL E")
	     ("f" . "MATHEMATICAL ITALIC SMALL F")
	     ("g" . "MATHEMATICAL ITALIC SMALL G")
	     ("h" . "PLANCK CONSTANT")
	     ("i" . "MATHEMATICAL ITALIC SMALL I")
	     ("j" . "MATHEMATICAL ITALIC SMALL J")
	     ("k" . "MATHEMATICAL ITALIC SMALL K")
	     ("l" . "MATHEMATICAL ITALIC SMALL L")
	     ("m" . "MATHEMATICAL ITALIC SMALL M")
	     ("n" . "MATHEMATICAL ITALIC SMALL N")
	     ("o" . "MATHEMATICAL ITALIC SMALL O")
	     ("p" . "MATHEMATICAL ITALIC SMALL P")
	     ("q" . "MATHEMATICAL ITALIC SMALL Q")
	     ("r" . "MATHEMATICAL ITALIC SMALL R")
	     ("s" . "MATHEMATICAL ITALIC SMALL S")
	     ("t" . "MATHEMATICAL ITALIC SMALL T")
	     ("u" . "MATHEMATICAL ITALIC SMALL U")
	     ("v" . "MATHEMATICAL ITALIC SMALL V")
	     ("w" . "MATHEMATICAL ITALIC SMALL W")
	     ("x" . "MATHEMATICAL ITALIC SMALL X")
	     ("y" . "MATHEMATICAL ITALIC SMALL Y")
	     ("z" . "MATHEMATICAL ITALIC SMALL Z")))
	  (mathematical-bold-letters
	   '(("A" . "MATHEMATICAL BOLD CAPITAL A")
	     ("B" . "MATHEMATICAL BOLD CAPITAL B")
	     ("C" . "MATHEMATICAL BOLD CAPITAL C")
	     ("D" . "MATHEMATICAL BOLD CAPITAL D")
	     ("E" . "MATHEMATICAL BOLD CAPITAL E")
	     ("F" . "MATHEMATICAL BOLD CAPITAL F")
	     ("G" . "MATHEMATICAL BOLD CAPITAL G")
	     ("H" . "MATHEMATICAL BOLD CAPITAL H")
	     ("I" . "MATHEMATICAL BOLD CAPITAL I")
	     ("J" . "MATHEMATICAL BOLD CAPITAL J")
	     ("K" . "MATHEMATICAL BOLD CAPITAL K")
	     ("L" . "MATHEMATICAL BOLD CAPITAL L")
	     ("M" . "MATHEMATICAL BOLD CAPITAL M")
	     ("N" . "MATHEMATICAL BOLD CAPITAL N")
	     ("O" . "MATHEMATICAL BOLD CAPITAL O")
	     ("P" . "MATHEMATICAL BOLD CAPITAL P")
	     ("Q" . "MATHEMATICAL BOLD CAPITAL Q")
	     ("R" . "MATHEMATICAL BOLD CAPITAL R")
	     ("S" . "MATHEMATICAL BOLD CAPITAL S")
	     ("T" . "MATHEMATICAL BOLD CAPITAL T")
	     ("U" . "MATHEMATICAL BOLD CAPITAL U")
	     ("V" . "MATHEMATICAL BOLD CAPITAL V")
	     ("W" . "MATHEMATICAL BOLD CAPITAL W")
	     ("X" . "MATHEMATICAL BOLD CAPITAL X")
	     ("Y" . "MATHEMATICAL BOLD CAPITAL Y")
	     ("Z" . "MATHEMATICAL BOLD CAPITAL Z")
	     ("a" . "MATHEMATICAL BOLD SMALL A")
	     ("b" . "MATHEMATICAL BOLD SMALL B")
	     ("c" . "MATHEMATICAL BOLD SMALL C")
	     ("d" . "MATHEMATICAL BOLD SMALL D")
	     ("e" . "MATHEMATICAL BOLD SMALL E")
	     ("f" . "MATHEMATICAL BOLD SMALL F")
	     ("g" . "MATHEMATICAL BOLD SMALL G")
	     ("h" . "MATHEMATICAL BOLD SMALL H")
	     ("i" . "MATHEMATICAL BOLD SMALL I")
	     ("j" . "MATHEMATICAL BOLD SMALL J")
	     ("k" . "MATHEMATICAL BOLD SMALL K")
	     ("l" . "MATHEMATICAL BOLD SMALL L")
	     ("m" . "MATHEMATICAL BOLD SMALL M")
	     ("n" . "MATHEMATICAL BOLD SMALL N")
	     ("o" . "MATHEMATICAL BOLD SMALL O")
	     ("p" . "MATHEMATICAL BOLD SMALL P")
	     ("q" . "MATHEMATICAL BOLD SMALL Q")
	     ("r" . "MATHEMATICAL BOLD SMALL R")
	     ("s" . "MATHEMATICAL BOLD SMALL S")
	     ("t" . "MATHEMATICAL BOLD SMALL T")
	     ("u" . "MATHEMATICAL BOLD SMALL U")
	     ("v" . "MATHEMATICAL BOLD SMALL V")
	     ("w" . "MATHEMATICAL BOLD SMALL W")
	     ("x" . "MATHEMATICAL BOLD SMALL X")
	     ("y" . "MATHEMATICAL BOLD SMALL Y")
	     ("z" . "MATHEMATICAL BOLD SMALL Z")))
	  (mathematical-bold-italic-letters
	   '(("A" . "MATHEMATICAL BOLD ITALIC CAPITAL A")
	     ("B" . "MATHEMATICAL BOLD ITALIC CAPITAL B")
	     ("C" . "MATHEMATICAL BOLD ITALIC CAPITAL C")
	     ("D" . "MATHEMATICAL BOLD ITALIC CAPITAL D")
	     ("E" . "MATHEMATICAL BOLD ITALIC CAPITAL E")
	     ("F" . "MATHEMATICAL BOLD ITALIC CAPITAL F")
	     ("G" . "MATHEMATICAL BOLD ITALIC CAPITAL G")
	     ("H" . "MATHEMATICAL BOLD ITALIC CAPITAL H")
	     ("I" . "MATHEMATICAL BOLD ITALIC CAPITAL I")
	     ("J" . "MATHEMATICAL BOLD ITALIC CAPITAL J")
	     ("K" . "MATHEMATICAL BOLD ITALIC CAPITAL K")
	     ("L" . "MATHEMATICAL BOLD ITALIC CAPITAL L")
	     ("M" . "MATHEMATICAL BOLD ITALIC CAPITAL M")
	     ("N" . "MATHEMATICAL BOLD ITALIC CAPITAL N")
	     ("O" . "MATHEMATICAL BOLD ITALIC CAPITAL O")
	     ("P" . "MATHEMATICAL BOLD ITALIC CAPITAL P")
	     ("Q" . "MATHEMATICAL BOLD ITALIC CAPITAL Q")
	     ("R" . "MATHEMATICAL BOLD ITALIC CAPITAL R")
	     ("S" . "MATHEMATICAL BOLD ITALIC CAPITAL S")
	     ("T" . "MATHEMATICAL BOLD ITALIC CAPITAL T")
	     ("U" . "MATHEMATICAL BOLD ITALIC CAPITAL U")
	     ("V" . "MATHEMATICAL BOLD ITALIC CAPITAL V")
	     ("W" . "MATHEMATICAL BOLD ITALIC CAPITAL W")
	     ("X" . "MATHEMATICAL BOLD ITALIC CAPITAL X")
	     ("Y" . "MATHEMATICAL BOLD ITALIC CAPITAL Y")
	     ("Z" . "MATHEMATICAL BOLD ITALIC CAPITAL Z")
	     ("a" . "MATHEMATICAL BOLD ITALIC SMALL A")
	     ("b" . "MATHEMATICAL BOLD ITALIC SMALL B")
	     ("c" . "MATHEMATICAL BOLD ITALIC SMALL C")
	     ("d" . "MATHEMATICAL BOLD ITALIC SMALL D")
	     ("e" . "MATHEMATICAL BOLD ITALIC SMALL E")
	     ("f" . "MATHEMATICAL BOLD ITALIC SMALL F")
	     ("g" . "MATHEMATICAL BOLD ITALIC SMALL G")
	     ("h" . "MATHEMATICAL BOLD ITALIC SMALL H")
	     ("i" . "MATHEMATICAL BOLD ITALIC SMALL I")
	     ("j" . "MATHEMATICAL BOLD ITALIC SMALL J")
	     ("k" . "MATHEMATICAL BOLD ITALIC SMALL K")
	     ("l" . "MATHEMATICAL BOLD ITALIC SMALL L")
	     ("m" . "MATHEMATICAL BOLD ITALIC SMALL M")
	     ("n" . "MATHEMATICAL BOLD ITALIC SMALL N")
	     ("o" . "MATHEMATICAL BOLD ITALIC SMALL O")
	     ("p" . "MATHEMATICAL BOLD ITALIC SMALL P")
	     ("q" . "MATHEMATICAL BOLD ITALIC SMALL Q")
	     ("r" . "MATHEMATICAL BOLD ITALIC SMALL R")
	     ("s" . "MATHEMATICAL BOLD ITALIC SMALL S")
	     ("t" . "MATHEMATICAL BOLD ITALIC SMALL T")
	     ("u" . "MATHEMATICAL BOLD ITALIC SMALL U")
	     ("v" . "MATHEMATICAL BOLD ITALIC SMALL V")
	     ("w" . "MATHEMATICAL BOLD ITALIC SMALL W")
	     ("x" . "MATHEMATICAL BOLD ITALIC SMALL X")
	     ("y" . "MATHEMATICAL BOLD ITALIC SMALL Y")
	     ("z" . "MATHEMATICAL BOLD ITALIC SMALL Z")))
	  (mathematical-double-struck-letters
	   '(("A" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL A")
	     ("B" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL B")
	     ("C" . "DOUBLE-STRUCK CAPITAL C")
	     ("D" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL D")
	     ("E" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL E")
	     ("F" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL F")
	     ("G" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL G")
	     ("H" . "DOUBLE-STRUCK CAPITAL H")
	     ("I" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL I")
	     ("J" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL J")
	     ("K" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL K")
	     ("L" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL L")
	     ("M" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL M")
	     ("N" . "DOUBLE-STRUCK CAPITAL N")
	     ("O" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL O")
	     ("P" . "DOUBLE-STRUCK CAPITAL P")
	     ("Q" . "DOUBLE-STRUCK CAPITAL Q")
	     ("R" . "DOUBLE-STRUCK CAPITAL R")
	     ("S" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL S")
	     ("T" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL T")
	     ("U" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL U")
	     ("V" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL V")
	     ("W" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL W")
	     ("X" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL X")
	     ("Y" . "MATHEMATICAL DOUBLE-STRUCK CAPITAL Y")
	     ("Z" . "DOUBLE-STRUCK CAPITAL Z")
	     ("a" . "MATHEMATICAL DOUBLE-STRUCK SMALL A")
	     ("b" . "MATHEMATICAL DOUBLE-STRUCK SMALL B")
	     ("c" . "MATHEMATICAL DOUBLE-STRUCK SMALL C")
	     ("d" . "MATHEMATICAL DOUBLE-STRUCK SMALL D")
	     ("e" . "MATHEMATICAL DOUBLE-STRUCK SMALL E")
	     ("f" . "MATHEMATICAL DOUBLE-STRUCK SMALL F")
	     ("g" . "MATHEMATICAL DOUBLE-STRUCK SMALL G")
	     ("h" . "MATHEMATICAL DOUBLE-STRUCK SMALL H")
	     ("i" . "MATHEMATICAL DOUBLE-STRUCK SMALL I")
	     ("j" . "MATHEMATICAL DOUBLE-STRUCK SMALL J")
	     ("k" . "MATHEMATICAL DOUBLE-STRUCK SMALL K")
	     ("l" . "MATHEMATICAL DOUBLE-STRUCK SMALL L")
	     ("m" . "MATHEMATICAL DOUBLE-STRUCK SMALL M")
	     ("n" . "MATHEMATICAL DOUBLE-STRUCK SMALL N")
	     ("o" . "MATHEMATICAL DOUBLE-STRUCK SMALL O")
	     ("p" . "MATHEMATICAL DOUBLE-STRUCK SMALL P")
	     ("q" . "MATHEMATICAL DOUBLE-STRUCK SMALL Q")
	     ("r" . "MATHEMATICAL DOUBLE-STRUCK SMALL R")
	     ("s" . "MATHEMATICAL DOUBLE-STRUCK SMALL S")
	     ("t" . "MATHEMATICAL DOUBLE-STRUCK SMALL T")
	     ("u" . "MATHEMATICAL DOUBLE-STRUCK SMALL U")
	     ("v" . "MATHEMATICAL DOUBLE-STRUCK SMALL V")
	     ("w" . "MATHEMATICAL DOUBLE-STRUCK SMALL W")
	     ("x" . "MATHEMATICAL DOUBLE-STRUCK SMALL X")
	     ("y" . "MATHEMATICAL DOUBLE-STRUCK SMALL Y")
	     ("z" . "MATHEMATICAL DOUBLE-STRUCK SMALL Z")
	     ("0" . "MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO")
	     ("1" . "MATHEMATICAL DOUBLE-STRUCK DIGIT ONE")
	     ("2" . "MATHEMATICAL DOUBLE-STRUCK DIGIT TWO")
	     ("3" . "MATHEMATICAL DOUBLE-STRUCK DIGIT THREE")
	     ("4" . "MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR")
	     ("5" . "MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE")
	     ("6" . "MATHEMATICAL DOUBLE-STRUCK DIGIT SIX")
	     ("7" . "MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN")
	     ("8" . "MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT")
	     ("9" . "MATHEMATICAL DOUBLE-STRUCK DIGIT NINE")))
	  (subscript-symbols
	   '(("0" . "SUBSCRIPT ZERO")
	     ("1" . "SUBSCRIPT ONE")
	     ("2" . "SUBSCRIPT TWO")
	     ("3" . "SUBSCRIPT THREE")
	     ("4" . "SUBSCRIPT FOUR")
	     ("5" . "SUBSCRIPT FIVE")
	     ("6" . "SUBSCRIPT SIX")
	     ("7" . "SUBSCRIPT SEVEN")
	     ("8" . "SUBSCRIPT EIGHT")
	     ("9" . "SUBSCRIPT NINE")
	     ("+" . "SUBSCRIPT PLUS SIGN")
	     ("-" . "SUBSCRIPT MINUS")
	     ("=" . "SUBSCRIPT EQUALS SIGN")
	     ("(" . "SUBSCRIPT LEFT PARENTHESIS")
	     (")" . "SUBSCRIPT RIGHT PARENTHESIS")
	     ("a" . "LATIN SUBSCRIPT SMALL LETTER A")
	     ("e" . "LATIN SUBSCRIPT SMALL LETTER E")
	     ("h" . "LATIN SUBSCRIPT SMALL LETTER H")
	     ("k" . "LATIN SUBSCRIPT SMALL LETTER K")
	     ("l" . "LATIN SUBSCRIPT SMALL LETTER L")
	     ("m" . "LATIN SUBSCRIPT SMALL LETTER M")
	     ("n" . "LATIN SUBSCRIPT SMALL LETTER N")
	     ("o" . "LATIN SUBSCRIPT SMALL LETTER O")
	     ("p" . "LATIN SUBSCRIPT SMALL LETTER P")
	     ("s" . "LATIN SUBSCRIPT SMALL LETTER S")
	     ("t" . "LATIN SUBSCRIPT SMALL LETTER T")
	     ("X" . "LATIN SUBSCRIPT SMALL LETTER X")))
	  (mathematical-operators
	   '(("A" . "FOR ALL")
	     ("E" . "THERE EXISTS")
	     ("nE" . "THERE DOES NOT EXIST")
	     ("0" . "EMPTY SET")
	     ("e" . "ELEMENT OF")
	     ("ne" . "NOT AN ELEMENT OF")
	     ("\\" . "SET MINUS")
	     ("-" . "MINUS SIGN")
	     ("(" . "SUBSET OF")
	     (")" . "SUPERSET OF")
	     ("[" . "SUBSET OF OR EQUAL TO")
	     ("]" . "SUPERSET OF OR EQUAL TO")
	     ("n[" . "NEITHER A SUBSET OF NOR EQUAL TO")
	     ("n]" . "NEITHER A SUPERSET OF NOR EQUAL TO")
	     ([right] . "RIGHTWARDS ARROW")
	     ([S-right] . "RIGHTWARDS DOUBLE ARROW")
	     ([C-right] . "LEFT RIGHT ARROW")
	     ([C-S-right] . "LEFT RIGHT DOUBLE ARROW")
	     ([left] . "LEFTWARDS ARROW")
	     ([S-left] . "LEFTWARDS DOUBLE ARROW")
	     ([M-right] . "LONG RIGHTWARDS ARROW")
	     ([M-S-right] . "LONG RIGHTWARDS DOUBLE ARROW")
	     ([M-left] . "LONG LEFTWARDS ARROW")
	     ([M-S-left] . "LONG LEFTWARDS DOUBLE ARROW")
	     ([up] . "UPWARDS ARROW")
	     ([down] . "DOWNWARDS ARROW")
	     ("<" . "LESS-THAN OR EQUAL TO")
	     (">" . "GREATER-THAN OR EQUAL TO")
	     ("x" . "MULTIPLICATION SIGN")
	     ("+" . "N-ARY SUMMATION")
	     ("*" . "N-ARY PRODUCT")
	     ("^" . "LOGICAL AND")
	     ("v" . "LOGICAL OR")
	     ("u" . "UNION")
	     ("U" . "N-ARY UNION")
	     ([?\M-u] . "SQUARE CUP")
	     ("i" . "INTERSECTION")
	     ("I" . "N-ARY INTERSECTION")
	     ([?\M-i] . "SQUARE CAP")
	     ("T" . "DOWN TACK")
	     ("L" . "UP TACK")
	     ("C" . "SQUARE IMAGE OF OR EQUAL TO")
	     (" f" . "FUNCTION APPLICATION")
	     (" *" . "INVISIBLE TIMES")
	     (" ," . "INVISIBLE SEPARATOR")
	     (" +" . "INVISIBLE PLUS")
	     ("8" . "INFINITY")
	     ("2" . "SQUARE ROOT")
	     ("|" . "DIVIDES")
	     ("=d" . "EQUAL TO BY DEFINITION")
	     ("=~" . "APPROXIMATELY EQUAL TO")
	     ([?= right] . "ASYMPTOTICALLY EQUAL TO")
	     ("n=" . "NOT EQUAL TO")
	     ("S" . "INTEGRAL")
	     ([?\M-=] . "IDENTICAL TO")
	     ([?n ?\M-=] . "NOT IDENTICAL TO")
	     ("=p" . "PROPORTIONAL TO")
	     ("~" . "TILDE OPERATOR")
	     ("'" . "PRIME")
	     ("m" . "MODELS")
	     ("nm" . "NOT TRUE")
	     ("p" . "RIGHT TACK")
	     ("np" . "DOES NOT PROVE")
	     ("o" . "CHECK MARK")
	     ([?\M-<] . "MATHEMATICAL LEFT ANGLE BRACKET")
	     ([?\M->] . "MATHEMATICAL RIGHT ANGLE BRACKET")
	     ("d" . "PARTIAL DIFFERENTIAL"))))
      (add-unicode-keymap greek-letters "g")
      (add-unicode-keymap greek-variant-letters "gv")
      (add-unicode-keymap mathematical-script-letters "ms")
      (add-unicode-keymap mathematical-italic-letters "mi")
      (add-unicode-keymap mathematical-bold-letters "mbr")
      (add-unicode-keymap mathematical-bold-italic-letters "mbi")
      (add-unicode-keymap mathematical-double-struck-letters "mds")
      (add-unicode-keymap subscript-symbols "_")
      (add-unicode-keymap mathematical-operators "mo"))))

(use-package font-latex
  :straight nil
  :after tex-site
  :config
  ;; Font for unicode mathematical symbols
  (create-fontset-from-fontset-spec "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-math")
  (set-fontset-font "fontset-math" 'unicode "XITS Math")
  (set-face-attribute 'font-latex-math-face nil :fontset "fontset-math"))

(use-package reftex
  :after tex-site
  :hook (LaTeX-mode . turn-on-reftex)	; Activate RefTeX mode
  :config
  ;; Use external programs instead of environment variables to find files
  (setq reftex-use-external-file-finders t)
  ;; Turn on all plug-ins for AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  ;; Add spanish words to ignore when deriving labels
  (setq reftex-spanish-ignorewords '("el" "la" "de"))
  (--each reftex-spanish-ignorewords
    (push it (nth 5 reftex-derive-label-parameters)))
  ;; Allow non-ASCII alphanumeric characters when deriving labels
  (setq reftex-translate-to-ascii-function nil)
  (setq reftex-label-illegal-re "[^-_+=:;,.[:alnum:]]"))
