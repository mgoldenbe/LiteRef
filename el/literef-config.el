(defcustom literef-directory "/home/meir/LiteRef/"
  "The root directory of the bibliography.")

(setq literef-papers-directory (concat literef-directory "papers/"))
(setq literef-drop-directory (concat literef-directory "drop/"))

(defcustom literef-pdf-viewer "evince"
  "The pdf viewer to be used.")

(defcustom literef-bibliography-style "plain"
  "The bibliography-style to be used for exports.")

;; Headlines are always sections, no matter the level.
;; (otherwise, when sections are deeply nested,
;; Latex creates numbered lists using alphabet, which may result
;; in the "counter too large" error.
(setq org-export-headline-levels most-positive-fixnum)

;; TODO words (e.g. the status of reading an article)
;; do not appear in the export
(setq org-export-with-todo-keywords nil)

;;;; BEGIN: Key bindings ----------------------------------------
(define-key global-map "\C-cw" 'literef-copy-current-key)
(define-key global-map "\C-co" 'literef-open-pdf)
(define-key global-map "\C-cs" 'literef-split-cite-title-author)
(define-key global-map "\C-cd" 'literef-split-cite)
(define-key global-map "\C-cp" 'literef-search-pdfs)
;;;; END --------------------------------------------------------

(provide 'literef-config)
