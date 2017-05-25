(require 'helm-bibtex)

(defcustom lite-ref-directory "/home/meir/LiteRef"
  "The root directory of LiteRef")

(defun literef-bib-files (&optional arg)
  "Compute the list of bib files"
  (file-expand-wildcards (concat lite-ref-directory "/papers/*/paper.bib"))
  )

(defun literef-helm-bibtex (&optional arg)
  "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread."

  (interactive "P")
  (let ; thanks to dynamic scoping, we can affect the variables used by helm-bibtex!
      (helm-source-fallback-options ; do not fall back onto online sources
       (bibtex-completion-display-formats '((t . "${author}, \"${title}\", ${journal}, ${year}")))
       (bibtex-completion-bibliography (literef-bib-files))
       (bibtex-completion-additional-search-fields '("journal"))
       )
    
    (helm-bibtex arg)))

