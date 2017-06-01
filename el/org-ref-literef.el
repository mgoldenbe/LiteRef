(defcustom literef-directory "/home/meir/LiteRef"
  "The root directory of the bibliography")

;;;; BEGIN: Set the bibliogaphy sources -------------------------
(defun literef-bib-files (&optional _arg)
  "Compute the list of bib files"
  (file-expand-wildcards (concat literef-directory "/papers/*/paper.bib"))
  )

(defun literef-set-default-bibliography(&optional _orig-fun)
  "Set the default bibliography."
  (setq org-ref-default-bibliography (literef-bib-files)))

;; Set the default bibliography at the beginning
(literef-set-default-bibliography)

;; advice org-ref-helm-insert-cite-link to begin by re-reading the default bibliography,
;; since entries could be added/removed.
(advice-add 'org-ref-helm-insert-cite-link :before #'literef-set-default-bibliography)

;; override this function to not offer adding an entry to
;; each bib file.
(defun bibtex-completion-fallback-candidates ()
  "Overrides bibtex-completion-fallback-candidates to not offer adding an entry to each bib file."
  bibtex-completion-fallback-options)
;;;; END --------------------------------------------------------

(defun literef-notes-filename(key)
  "Compute name of the notes file based on the key"
  (concat literef-directory "/papers/" key "/paper.org"))


(defun literef-open-notes()
  "Open notes for the cite link under cursor"
  (let*  ((key (org-ref-get-bibtex-key-under-cursor))
	  (filename (literef-notes-filename key)))
	  (find-file-other-window filename)))

; The default value of this one is org-ref-cite-click-helm
(setq org-ref-cite-onclick-function (lambda(_key) (literef-open-notes)))
