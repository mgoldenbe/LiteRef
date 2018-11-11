;;; literef-config.el --- setting values of configuration variables and key shortcuts for frequently used commands.

;; Copyright(C) 2017-2018 Meir Goldenberg

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module defines the variables the control the functionalities
;; of LiteRef and sets their values. It defines key shortcuts for
;; frequently used commands as well. The user is welcome to set these
;; variables and shortcuts to suit his needs and taste.

;;; Code:
(defcustom literef-papers-directory (concat literef-directory "papers/")
  "The folder containing the papers database.")
(mkdir literef-papers-directory t)

(defcustom literef-drop-directory (concat literef-directory "drop/")
  "The folder for dropping new files, such as downloaded BibTeX entries and
  PDF files. LiteRef uses this folder for files representing server
  requests as well.")
(mkdir literef-drop-directory t)

(defcustom literef-survey-directory (concat literef-directory "survey/")
  "The folder used by default for exporting the survey.")
(mkdir literef-survey-directory t)

(defcustom literef-equal-timestamps 5
  "The number of seconds, within which creation timestamps of two
  papers database entries corresponding to two candidates in helm are
  considered to be same with respect to sorting the candidates.")

(defcustom literef-sort-citation-links t
  "Determines whether citation links should be sorted on-the-fly.")

(defcustom literef-citation-link-sorting-criteria "k"
  "The sorting criteria for automatic sorting of citation links. It is
  a string consisting of comma-separated characters that stand for sorting
  criteria. The following characters in parentheses can be used: Key↑
  (k), Key↓ (K), Creation Date↑ (d), Creation Date↓ (D), Author↑ (a),
  Author↓ (A), Title↑ (t), Title↓ (T), Venue↑ (v), Venue↓ (V), Venue
  Type↑ (w), Venue Type↓ (W), Year↑ (y), Year↓ (Y).")

(defcustom literef-bibliography-package "natbib"
  "The bibliography package to be used for LaTeX export. If nil, no
  package specification is inserted into the document.")

(defcustom literef-bibliography-style "plainnat"
  "The bibliography style to be used for LaTeX export. If nil, no
  bibliography style specification is inserted into the document.")

(defcustom literef-citation-function-link "f"
  "The link name for specifying citation functions.")

(defcustom literef-citation-function-color "red"
  "The color of citation function links.")

(defcustom literef-pdf-annotation-link "annot"
  "The link name for citing a PDF annotation.")

(defcustom literef-pdf-annotation-color "chocolate"
  "The color of PDF annotation links.")

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
  "Types of citation functions used for annotating citations.")

(defcustom literef-arc-filter-variables-prefix ""
  "The prefix used for the names of the special variables in the
  specification of the arc filter. See `literef-arc-filter-variables'
  and `literef-arc-filter-temp-variable'.")

(defcustom literef-graph-font-height 120
  "The font height used in the textual visualization of the selected subgraph.")

(defcustom literef-subgraph-show-only-generating-arcs nil
  "Determines whether the set of arcs in the visualization of the selected subgraph should be restricted to the arcs traversed by the uniform-cost search that built the subgraph.")

(defcustom literef-subgraph-show-buffer nil
  "Determines whether a node corresponding to the source buffer is to be shown in the subgraph visualization.")

(defcustom literef-default-image-latex-attrs ":width 0.9\\\\linewidth"
  "The default attributes for an inline image for LaTeX export. See `https://www.gnu.org/software/emacs/manual/html_node/org/LaTeX-specific-attributes.html' for the description of the possible attributes. Note that, when backslash is used, four backslashes need to be typed.")

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
(define-key global-map "\C-c]" 'org-ref-insert-link)
(define-key global-map "\C-c)" 'literef-subgraph-helm)


;;;; END --------------------------------------------------------

(provide 'literef-config)
