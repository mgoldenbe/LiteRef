(defcustom literef-directory "/home/meir/LiteRef/"
  "The root directory of the bibliography")

(setq literef-papers-directory (concat literef-directory "papers/"))
(setq literef-drop-directory (concat literef-directory "drop/"))

(defcustom literef-pdf-viewer "evince"
  "The pdf viewer to be used")

;;;; BEGIN: Set the bibliogaphy sources -------------------------
(defun literef-bib-files (&optional _arg)
  "Compute the list of bib files"
  (sort (file-expand-wildcards (concat literef-papers-directory "*/paper.bib")) 'string<))

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

;;;; BEGIN: Opening various files associated with a paper -------
(defun literef-get-bibtex-key-under-cursor()
  "Non-throwing version of org-ref-get-bibtex-key-under-cursor.

Also, this version does not affect point"
  (save-excursion
    (condition-case nil (org-ref-get-bibtex-key-under-cursor) (error nil))))

(defun literef-filename(key ext)
  "Compute name of a file with the given extension pertaining to a paper with the given key."
  (concat literef-papers-directory key (concat "/paper." ext)))

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

; The default value of this one is org-ref-cite-click-helm
(setq org-ref-cite-onclick-function (lambda(_key) (literef-open-notes)))

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html
(defun literef-current-folder-key()
  "Compute key based on the folder in which the file being visited is located.

If there the visited folder does not correspond to a key, returns nil."
  (let ((key (car (last (nbutlast (split-string (file-name-directory buffer-file-name) "/"))))))
    (if (file-directory-p (concat literef-papers-directory key))
	key
      nil)))

(defun literef-open-pdf()
  "Open the pdf for the citation under cursor or for the paper of the current notes file."
  (interactive)
  (let ((key (or (literef-get-bibtex-key-under-cursor)
		 (literef-current-folder-key))))
    (when (not (eq key nil))
      (let ((filename (literef-pdf-filename key)))
	(if (file-exists-p filename)
	    (shell-command (concat literef-pdf-viewer " " filename))
	  (with-temp-file (literef-request-filename)
	    (insert (concat "getPdf" " " key))))))))

;;;; END --------------------------------------------------------

(defun literef-split-cite()
  "Split citation of multiple sources
  
Splits the first citation of multiple sources found on the current line, so that each souce appears on a separate line, while the text preceeding and succeeding the long citation is duplicated on each line"
  (interactive)
  (let* ((save-point (point)) ; because we'll kill the line
	 (postfix-end (progn (end-of-line) (point)))
	 (prefix-begin (progn (beginning-of-line) (point)))
	 (prefix-end (progn (search-forward "cite:" postfix-end t) (point)))
	 (prefix (buffer-substring prefix-begin prefix-end)))
    (if (= prefix-begin prefix-end) 
	(message "No citation on this line")
      (let* ((cite (org-element-context))
	     (keys-with-commas (org-element-property :path cite))
	     (keys (org-ref-split-and-strip-string keys-with-commas))
	     (postfix-begin (+ prefix-end (length keys-with-commas)))
	     (postfix (buffer-substring postfix-begin postfix-end)))
	;; remove the original line
	(kill-region prefix-begin postfix-end)

	;; insert a line for each key
	(dolist (key keys nil)
	  (insert prefix key postfix "\n"))

	;; restore the point
	(goto-char save-point)
	))))

;;;; BEGIN: Key bindings ----------------------------------------
(define-key global-map "\C-co" 'literef-open-pdf)
(define-key global-map "\C-cs" 'literef-split-cite)
;;;; END --------------------------------------------------------
