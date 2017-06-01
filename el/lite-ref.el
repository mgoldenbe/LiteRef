(require 'helm-bibtex)

(defcustom lite-ref-directory "/home/meir/LiteRef"
  "The root directory of LiteRef")

(defun literef-bib-files (&optional arg)
  "Compute the list of bib files"
  (file-expand-wildcards (concat lite-ref-directory "/papers/*/paper.bib"))
  )

(defun literef-insert-reference (keys)
  "A modification of bibtex-completion-insert-reference. 

Works with a single reference and does not add extra stuff."
  (let*
      ((key (car keys))
       (file (concat lite-ref-directory "/papers/" key "paper.org"))
       (text key)
       (help (bibtex-completion-apa-format-reference key))
       (beg-pos (point)))
    (insert (concat "[[file:" file "][" text "]]"))
    (let ((end-pos (point)))
      (add-text-properties beg-pos end-pos '(help-echo ,"aaa")))))

(helm-bibtex-helmify-action literef-insert-reference literef-insert-reference-helmified)

(defun literef-source-bibtex()
  "Returns a modified value of the helm-source-bibtex variable.

Actions are LiteRef-specific."
  (helm-build-sync-source "BibTeX entries"
    :init 'bibtex-completion-init
    :candidates 'bibtex-completion-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
             "Insert reference"           'literef-insert-reference-helmified
             "Insert BibTeX key"          'helm-bibtex-insert-key)))

(defun literef-helm-bibtex (&optional arg)
  "LiteRef wrapper around helm-bibtex. 

The purpose of this wrapper is to pass the bib files stored in the papers/ directory, to modify how the selection choices are displayed and to modify the list of actions.

helm-bibtex uses bibtex-completion-apa-format-reference to format references."

  (interactive "P")
  (let ; thanks to dynamic scoping, we can affect the variables used by helm-bibtex!
      (helm-source-fallback-options ; do not fall back onto online sources
       (bibtex-completion-display-formats '((t . "${author}, \"${title}\", ${journal}, ${year}")))
       (bibtex-completion-bibliography (literef-bib-files))
       (bibtex-completion-additional-search-fields '("journal"))
       (helm-source-bibtex (literef-source-bibtex))
       )
    
    (helm-bibtex t)))

