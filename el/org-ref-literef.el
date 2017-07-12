(require 'latex-map)

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
  (let ((key (literef-current-key)))
    (when key
      (let ((filename (literef-pdf-filename key)))
	(if (file-exists-p filename)
	    (shell-command (concat literef-pdf-viewer " " filename))
	  (with-temp-file (literef-request-filename)
	    (insert (concat "getPdf" " " key))))))))
;;;; END --------------------------------------------------------

;;;; BEGIN: Copying and pasting key(s) --------------------------
(defun literef-copy-current-key()
  "Copy the current key for later intelligent yanking."
  (interactive)
  (let ((key (literef-current-key)))
    (when key (kill-new key))))

(defun literef-insert-for-yank (orig-fun string)
  "The version of insert-for-yank that handles the keys being yanked intelligently.

The string to be yanked is preceeded by the prefix computed as follows:

1. Split the contents being yanked based on commas and analyze the first entry. If it is not a valid key, the prefix is empty. Otherwise, proceed to step 2. 

2. If the point is right after 'cite:', the prefix is empty. Otherwise, proceed to step 3. 

3. If the point is over a key, the prefix is ','. Otherwise, the prefix is 'cite:'"
  
  (let* ((key (car (split-string string ",")))
	 (prefix 
	  (if (literef-key-exists key)
	      (cond ((and
		(> (point) 4)
		(string= (buffer-substring (- (point) 5) (point)) "cite:"))
		     ;; We are after 'cite:'
		     "")
		    ((literef-get-bibtex-key-under-cursor)
		     ;; We are over a key
		     ",")
		    (t
		     ;; Else
		     "cite:"))
	    "")))
    (funcall orig-fun (concat prefix string))
    ))

(advice-add 'insert-for-yank :around #'literef-insert-for-yank)
;;;; END --------------------------------------------------------

;;;; BEGIN: Splitting a citation --------------------------------
(defun literef-split-cite-raw(insert-title-author)
  "Split citation of multiple sources. Insert information about title and author before the key if INSERT-TITLE-AUTHOR is not nil.
  
Splits the first citation of multiple sources found on the current line, so that each souce appears on a separate line, while the text preceeding and succeeding the long citation is duplicated on each line"
  (interactive)
  (let* ((save-point (point)) ; because we'll kill the line
	 (postfix-end (progn (end-of-line) (point)))
	 (prefix-begin (progn (beginning-of-line) (point)))
	 (prefix-end (progn (search-forward "cite:" postfix-end t) (point)))
	 (prefix (buffer-substring prefix-begin (- prefix-end 5))))
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
	  ;; get title and authors
	  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
		 (entry (bibtex-completion-get-entry key))
		 (authors
		  (literef-translate-latex
		   (bibtex-completion-apa-get-value "author" entry)))
		 (title
		  (literef-translate-latex
		   (bibtex-completion-get-value "title" entry)))
		 (title-author (and insert-title-author (concat  "\"" title "\" by " authors))))
	    (unless (string= key (car keys)) (insert "\n"))
	    (insert prefix (concat title-author) postfix " cite:" key)))))
	  
    (goto-char save-point)))

(defun literef-split-cite-title-author()
  "Split citation of multiple sources. Insert information about title and author before the key.
  
Splits the first citation of multiple sources found on the current line, so that each souce appears on a separate line, while the text preceeding and succeeding the long citation is duplicated on each line."
  (interactive)
  (literef-split-cite-raw t))

(defun literef-split-cite()
  "Split citation of multiple sources.
  
Splits the first citation of multiple sources found on the current line, so that each souce appears on a separate line, while the text preceeding and succeeding the long citation is duplicated on each line."
  (interactive)
  (literef-split-cite-raw nil))
;;;; END --------------------------------------------------------

;;;; BEGIN: Searches --------------------------------------------
(defun literef-search-pdfs(string)
  "Yank the citation links obtained by searching for a given string in the first page of the paper PDFs.

The function relies on the `pdfgrep` shell command. The string pattern must be a valid pattern for that command."
  (interactive "sSearch pattern: ")
  (let* ((command (concat "pdfgrep -Hno -m 1" " " string " " literef-papers-directory "*/paper.pdf | grep '1:'"))
	 (raw-output (substring (shell-command-to-string command) 0 -1))
	 (output (split-string raw-output "\n")))
    (dolist (line output nil)
      (let* ((prefix-length (length literef-papers-directory))
	     (line-no-prefix (substring line prefix-length))
	     (key (car (split-string line-no-prefix "/"))))
	(insert-for-yank key)))))
;;;; END --------------------------------------------------------

;;;; BEGIN: Export ----------------------------------------------
(defun literef-make-bib-file(&rest _args)
  "Make the bibliography file containing only the entries for the used keys."
  (with-temp-file bib-file
    (dolist (key (delete-dups (sort exported-keys 'string<)) nil)
      (let ((cur-bib (literef-bib-filename key)))
	(and
	 (file-exists-p cur-bib)
	 (insert-file-contents cur-bib)))
      (goto-char (point-max))
      (insert "\n"))))
  
(defun literef-export-to-file(orig-fun type file &rest args)
  "Export to latex that adds bibliography."
  (save-excursion
    (with-silent-modifications
      (let* ((bib-file (concat (substring file 0 -3) "bib"))
	     (exported-keys nil)
	     (beg (point-max))
	     (append-text (concat "\n" "bibliographystyle:" "plain" "\n"
				  "bibliography:" bib-file))
	     (end (+ beg (length append-text))))
	(when (file-exists-p bib-file) (delete-file bib-file))
	(goto-char beg)
	(insert append-text)
	(apply orig-fun (cons type (cons file args)))
	(delete-region beg end)
        ;; if there was no post-processing in 'orig-fun,
	;; so that the bib file was not created, then create it now.
	(when (not (file-exists-p bib-file)) (literef-make-bib-file))))))

(defun literef-cite-export(key desc backend)
  "The function for exporting a citation link."
  (setq exported-keys (cons key exported-keys)))

(advice-add 'org-export-to-file :around #'literef-export-to-file)
(advice-add 'org-latex-compile :before #'literef-make-bib-file)
(advice-add (org-link-get-parameter "cite" :export) :after #'literef-cite-export)
;;;; END --------------------------------------------------------

;;;; BEGIN: Key bindings ----------------------------------------
(define-key global-map "\C-cw" 'literef-copy-current-key)
(define-key global-map "\C-co" 'literef-open-pdf)
(define-key global-map "\C-cs" 'literef-split-cite-title-author)
(define-key global-map "\C-cd" 'literef-split-cite)
(define-key global-map "\C-cp" 'literef-search-pdfs)
;;;; END --------------------------------------------------------


;;;; BEGIN: Annotations -----------------------------------------
(defun annotation-help-echo(window object position)
  (save-excursion
    (goto-char position)
    (goto-char (org-element-property :end (org-element-context)))
    (format "%s %s %s" "The current paper" (match-string 2) "the ideas of the cited one.")))
    
(org-link-set-parameters
 "annotation"
 :follow (lambda (path) (message "%s %s %s" "The current paper" path "the ideas of the cited one."))
 :export (lambda (path desc backend) "") ; ignore the link
 :face '(:foreground "red")
 :help-echo 'annotation-help-echo)
;;;; END --------------------------------------------------------
