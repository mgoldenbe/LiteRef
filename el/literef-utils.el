(defun literef-bib-files (&optional _arg)
  "Compute the list of bib files."
  (sort (file-expand-wildcards (concat literef-papers-directory "*/paper.bib")) 'string<))

(defun literef-set-default-bibliography(&optional _orig-fun)
  "Set the default bibliography."
  (setq org-ref-default-bibliography (literef-bib-files)))

;;;; BEGIN: Constant-time look-up of entries --------------------
(defun literef-get-entry (orig-fun &rest args)
  "The version of bibtex-completion-get-entry1 with the only source set to the bib file corresponding to the given key."
  (let ((bibtex-completion-bibliography (list (literef-bib-filename entry-key))))
    (apply orig-fun args)))

(advice-add 'bibtex-completion-get-entry1 :around #'literef-get-entry)
;;;; END --------------------------------------------------------

;;;; BEGIN: Opening various files associated with a paper -------
(defun literef-get-bibtex-key-under-cursor()
  "Non-throwing version of org-ref-get-bibtex-key-under-cursor.

Also, this version does not affect point"
  (save-excursion
    (condition-case nil (org-ref-get-bibtex-key-under-cursor) (error nil))))

(defun literef-current-key()
  "The key under cursor or for the paper of the current notes or .bib file.

Returns nil if neither of these ways produces a key."
  (or (literef-get-bibtex-key-under-cursor)
      (literef-current-folder-key)))

(defun literef-key-exists(key)
  "Return t if key exists and nil otherwise."
  (let ((filename (literef-bib-filename key)))
    (file-exists-p filename)))

(defun literef-filename(key ext)
  "Compute name of a file with the given extension pertaining to a paper with the given key."
  (concat literef-papers-directory key (concat "/paper." ext)))

(defun literef-bib-filename(key)
  "Compute name of the .bib file based on the key and the extension"
  (literef-filename key "bib"))

(defun literef-notes-filename(key)
  "Compute name of the notes file based on the key and the extension"
  (literef-filename key "org"))

(defun literef-pdf-filename(key)
  "Compute name of the pdf file based on the key and the extension"
  (literef-filename key "pdf"))

(defun literef-request-filename()
  "Compute name of the request file"
  (concat literef-drop-directory "request." (number-to-string (float-time)) ".rqt"))
  
(defun literef-open-notes()
  "Open notes for the cite link under cursor"
  (let*  ((key (org-ref-get-bibtex-key-under-cursor))
	  (filename (literef-notes-filename key)))
    (find-file-other-window filename)))

(defun literef-current-folder-key()
  "Compute key based on the folder in which the file being visited is located.

If there the visited folder does not correspond to a key, returns nil."
  (let ((key (car (last (nbutlast (split-string (file-name-directory buffer-file-name) "/"))))))
    (if (file-directory-p (concat literef-papers-directory key))
	key
      nil)))

(provide 'literef-utils)
