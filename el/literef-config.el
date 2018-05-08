(setq literef-papers-directory (concat literef-directory "papers/"))
(setq literef-drop-directory (concat literef-directory "drop/"))

(defcustom literef-equal-timestamps 5
  "The number of seconds, within which entries for two papers as candidates in helm are considered to have been created at the same time.")

(defcustom literef-sort-citation-links t
  "Determines whether the citation links should be automatically sorted.")

(defcustom literef-citation-link-sorting-criteria "k"
  "The sorting criteria for automatic sorting of citation links. It is a comma-separated list of characters as in `literef-char-to-compare'.")

(defcustom literef-bibliography-package "natbib"
  "The bibliography package to be used for LaTeX export. If nil, no package is added.")

(defcustom literef-bibliography-style "plainnat"
  "The bibliography style to be used for LaTeX export. If nil, no bibliography style is added.")

(defcustom literef-citation-function-link "f"
  "The link name for specifying citation function.")

(defcustom literef-citation-function-color "red"
  "The color of citation function links.")

(defcustom literef-pdf-annotation-link "annot"
  "The link name for citing a pdf annotation.")

(defcustom literef-pdf-annotation-color "chocolate"
  "The color of pdf annotation links.")

(defcustom literef-citation-functions
  '(
    "develops-ideas"
    "resembles-ideas"
    "applies-to-domain"
    "compares-results"
    "compares-applicability"
    "claims-orthogonality"
    "shares-goals"
    "analyzes-theoretically")
  "Kinds of citation functions used for annotation.")

(defcustom literef-arc-filter-variables-prefix ""
  "The prefix used for special variables in the specification of the arc filter.")

(defcustom literef-graph-font-height 120
  "The font height used when displaying the current subgraph selection.")

(defcustom literef-subgraph-show-only-generating-arcs nil
  "Determines whether only the generating arcs of the subgraph should be visualized.")

(defcustom literef-subgraph-show-buffer nil
  "Determines whether the buffer source node is to be shown in the visualization.")

(defcustom literef-default-image-latex-attrs ":width 0.9\\\\linewidth"
  "The default attributes for an inline image for LaTeX export. See `https://www.gnu.org/software/emacs/manual/html_node/org/LaTeX-specific-attributes.html' for the description of the possible attributes. Note that, when backslash is used, four backslashes need to be put.")

(defcustom literef-default-image-html-attrs ":width 90%"
  "The default attributes for an inline image for HTML export. See `https://www.gnu.org/software/emacs/manual/html_node/org/Images-in-HTML-export.html' and `https://orgmode.org/worg/org-tutorials/images-and-xhtml-export.html' for the description of the possible attributes.")

;; Headlines are always sections, no matter the level.
;; (otherwise, when sections are deeply nested,
;; Latex creates numbered lists using alphabet, which may result
;; in the "counter too large" error.
(setq org-export-headline-levels most-positive-fixnum)

;; TODO words (e.g. the status of reading an article)
;; do not appear in the export
(setq org-export-with-todo-keywords nil)

;;;; BEGIN: Key bindings ----------------------------------------
(define-key global-map (kbd "C-c <down>") 'literef-sort-citation-link)
(define-key global-map "\C-cc" 'literef-bibtex-from-clipboard)
(define-key global-map "\C-cw" 'literef-copy-current-key)
(define-key global-map "\C-cf" 'literef-citation-function)
(define-key global-map "\C-cb" 'literef-open-bibfile)
(define-key global-map "\C-cn" 'literef-open-notes)
(define-key global-map "\C-co" 'literef-open-pdf)
(define-key global-map "\C-cs" 'literef-split-cite-title-author)
(define-key global-map "\C-cd" 'literef-split-cite)
(define-key global-map "\C-ca" 'literef-cite-pdf-annotation)
(define-key global-map "\C-cg" 'literef-get-region-bibtex)
(define-key global-map "\C-cp" 'literef-search-pdfs)
(define-key global-map "\C-cu" 'literef-select-subgraph)
(define-key global-map "\C-cv" 'literef-show-selected-subgraph)
(define-key global-map "\C-c)" 'literef-subgraph-helm)


;;;; END --------------------------------------------------------

(provide 'literef-config)
