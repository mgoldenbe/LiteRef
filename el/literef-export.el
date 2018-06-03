(defun literef-key-notes-p(key)
  "Predicate showing whether the notes for KEY are to be inserted.

Currently it returns t if the notes file is of non-zero size."
  (let ((notes-file (literef-notes-filename key))
	(index-of-size-attribute 7))
    (> (elt (file-attributes notes-file) index-of-size-attribute) 0)))

(defvar literef-no-section-reference " <noref>"
  "A sign that the preceding citation should not be followed by a section reference. This is currently used to avoid inserting self-references in the section headings.")

(defun literef-reference-text(keys)
  "Compute text for the reference for the given KEYS."

  (let* ((keys ;; only the keys whos notes are exported.
	  (let (res)
	    (dolist (key keys nil)
	      (when
		  (and (literef-key-in-subgraph-p key)
		       (literef-key-notes-p key))	 
		(push key res)))
	    (reverse res)))
	 (multiple-flag (> (length keys) 1))
	 (res (if keys " (Section" "")))
    (when multiple-flag (setq res (concat res "s")))
    (when keys (setq res (concat res " ")))
    (let ((keys-remain (length keys)))
      (dolist (key keys nil)
	(setq res (concat res "[[sec:" key "]]"))
	(setq keys-remain (1- keys-remain))
	(let ((separator
	       (cond ((< keys-remain 1) "")
		     ((> keys-remain 1) ", ")
		     (t " and "))))
	  (setq res (concat res separator)))))
    (when keys (setq res (concat res ")")))
    res))

(defun literef-debug-print-buffer-citations()
  "Displays a string summarizing the citations in the buffer for debugging."
  (interactive)
  (let (res)
    (dolist (link (literef-citation-links) nil)
      (setq res (concat res "(begin: " (number-to-string (literef-link-begin link)) ", end: " (number-to-string (literef-link-end link)) ")    ")))
    (message res)))

(defun literef-remove-citation-functions()
  "Remove all citation function links and space before them"
  (dolist (link (reverse (literef-citation-function-links)) nil)
    (let* ((prev-non-space (literef-link-prev-non-space link))
	   (begin
	    (if prev-non-space
		(1+ prev-non-space)   
	      (literef-link-begin link)))
	   (end (literef-link-end link)))
      (delete-region begin end))))

(defun literef-insert-note-references()
  "For each citation in the current buffer, insert a reference to the sections corresponding to the keys in the selected subgraph. Properly handle comma-separated citations."
  (dolist (link (reverse (literef-citation-links)) nil)
    (let* ((keys (literef-link-path-components link))
	   (reference (literef-reference-text keys)))
	(goto-char (literef-link-end link))
	(let* ((begin (point))
	       (end (+ begin (length literef-no-section-reference))))
	  (if (and (< end (point-max))
		   (equal (buffer-substring begin end)
			  literef-no-section-reference))
	      (delete-region begin end)
	    (insert reference))))))

(defun literef-make-bib-file(bib-file-name)
  "Make the bibliography file containing only the entries for the used keys."
  (let ((keys (literef-buffer-keys)))
    (when (file-exists-p bib-file-name) (delete-file bib-file-name))
    (when literef-sort-citation-links
      (setq keys (literef-sort-keys 
		  keys
		  literef-citation-link-sorting-criteria)))
    (with-temp-file bib-file-name
      (dolist (key keys nil)
	(let ((cur-bib (literef-bib-filename key)))
	  (and
	   (file-exists-p cur-bib)
	   (insert-file-contents cur-bib)))
	(goto-char (point-max))
	(insert "\n")))))

(defun literef-keys-to-insert()
  "Compute the list of keys whose notes are to be inserted in the export."
  (let (res)
    (dolist (key (literef-hash-keys-to-list (literef-subgraph-keys)) nil)
      (when (and (literef-key-exists key) (literef-key-notes-p key))
	(push key res)))
    (sort res 'string<)))

(defun literef-export-file-name-without-ext(orig-file-name)
  "Returns the file name for export without extension based on the original file name ORIG-FILE-NAME passed to `literef-export-to-file'."
  (let ((res
	 (when (boundp 'literef-subgraph-export)
	   (let ((source-type (literef-subgraph-source-property :source-type)))
	     (cond
	      ((eq source-type :buffer)
	       (literef-subgraph-source-property :file-name))
	      ((member source-type '(:current-key :all-keys))
	       (read-file-name
		"Choose the document name (no extension): "
		(if (eq source-type :current-key) 
		    (file-name-directory
		     (literef-notes-filename
		      (literef-subgraph-source-property :source-name)))
		  literef-survey-directory)
		nil nil
		"survey")))))))
    (unless res (setq res orig-file-name))
    (file-name-sans-extension (expand-file-name res))))

(defun literef-source-buffer-string()
  "Returns the contents of the source buffer:
-- If not exporting the selected sub-graph, return contents of the current buffer.
-- If the source is a paper or the whole citation graph, then return empty string. 
-- If the source buffer of the selected sub-graph exists and is visiting the source file, then the buffer's contents is returned. 
-- Otherwise, return the source file's contents if that file exists. 
-- Report an error."
  (let* ((res
	  (unless (boundp 'literef-subgraph-export)
	    (buffer-string))))
    (unless res ;; exporting the selected sub-graph
      (let* ((source-type
	      (literef-subgraph-source-property :source-type))
	     (key
	      (when (eq source-type :current-key)
		(literef-subgraph-source-property :source-name)))
	     (buffer
	      (when (eq source-type :buffer)
		(get-buffer
		 (literef-subgraph-source-property :source-name))))
	     (file-name
	      (when (eq source-type :buffer)
		(literef-subgraph-source-property :file-name))))
	(when (or key (eq source-type :all-keys))
	  (setq res ""))
	(when buffer
	  (with-current-buffer buffer
	    (when (equal file-name (expand-file-name (buffer-file-name)))
	      (setq res (buffer-string)))))
	(unless res
	  (with-temp-buffer
	    (when (not (file-exists-p file-name))
	      (error (error (concat "The source does not exist anymore."))))
	    (insert-file-contents file-name)
	    (setq res (buffer-string))))))
    res))

(defun literef-export-to-file(orig-fun backend file &rest args)
  "The version of `org-export-to-file' that supports exporting the selected sub-graph. BACKEND and FILE are as in `org-export-to-file'.

It performs some pre-processing and then calls ORIG-FUN, which is the original `org-export-to-file'. When exporting the selected sub-graph, the pre-processing includes computing the file name for export to be passed to `org-export-to-file'."
  (let* ((current-subgraph literef-subgraph)
	 (file-no-ext (literef-export-file-name-without-ext file))
	 (extension (file-name-extension file))
	 (file (concat file-no-ext "." extension))
	 (buffer-string (literef-source-buffer-string)))
    (with-temp-buffer
      (org-mode)
      
      ;; Insert contents of a buffer if necessary.
      (when (and (stringp buffer-string)
		 (> (length buffer-string) 0))
	(when (boundp 'literef-subgraph-export)
	  (let* ((default-section-name
		   (literef-subgraph-source-property :buffer-node-name))
		 (section-name "nil") ;; no section for buffer.
		  ;; (read-string
		  ;;  (concat "Enter the title of the buffer secion "
		  ;; 	   "(default: " default-section-name
		  ;; 	   ") or nil to not create a section: ")
		  ;;  nil nil default-section-name))
		 )
	    (unless (equal section-name "nil")
	      (insert "* "  section-name "\n"))))
	(insert buffer-string)
	(unless (eq (substring buffer-string -1) "\n") (insert "\n")))

      ;; Now handle the insertion of notes.
      (when (boundp 'literef-subgraph-export)
	(let ((keys (literef-keys-to-insert)))
	  (when (> (length keys) 0)
	    (let* ((create-notes-section
		   (y-or-n-p
		    "Should there be a section for all the notes?"))
		   (notes-stars (if create-notes-section "**" "*")))
	    (when create-notes-section (insert "* Notes\n"))
	    (dolist (key keys nil)
	      (let ((notes-file (literef-notes-filename key)))
		(insert notes-stars " " (literef-key-string key)
			" (cite:" key literef-no-section-reference ")\n"
			"<<sec:" key ">>" "\n")
		(insert "#+INCLUDE: " notes-file "\n")))))))
		
      ;; Expand INCLUDEs.
      (org-export-expand-include-keyword)
      (end-of-buffer)

      ;; Sort citation links.
      (when literef-sort-citation-links (literef-sort-citation-links t))

      ;; Remove citation functions and the spaces preceeding them.
      (literef-remove-citation-functions)
      
      ;; Insert references to note sections.
      (when (boundp 'literef-subgraph-export)
	(literef-insert-note-references))
      (end-of-buffer)

      ;; Insert bibliography if needed.
      (when (and literef-bibliography-style
		 (> (length (literef-buffer-keys)) 0))
	(let* ((bib-file-name (concat file-no-ext ".bib"))
	       (append-text (concat "\n" "bibliographystyle:"
				    literef-bibliography-style "\n"
				    "bibliography:" bib-file-name)))
	  (insert append-text)
	  (literef-make-bib-file bib-file-name)))

      ;; Insert bibliography package
      (when literef-bibliography-package
       	(goto-char (point-min))
	(insert "#+LATEX_HEADER: \\usepackage{" literef-bibliography-package "}\n"))
      
      (apply orig-fun backend file args))))

(defun literef-subgraph-export-dispatch()
  "The version of `org-export-dispatch' that works with the selected subgraph."
  (interactive)
  (let ((literef-subgraph-export t))
    (org-export-dispatch)))

(defun literef-subgraph-image-file-link
    (key-or-file literef-subgraph-show-buffer buffer-node-name filter
		 caption name attr-latex attr-html)
  "Create an image for the subgraph with the source specified by KEY-OR-FILE and the filter string FILTER, while respecting the given `literef-subgraph-show-buffer'. Return a link to the created image. The caption, name and export attributes are set using the corresponding arguments."
  (let* ((literef-subgraph
	  (literef-select-subgraph-for-export
	   key-or-file filter buffer-node-name))
	 (image-file (literef-subgraph-save-image "png")))
    (concat
      (format "#+CAPTION: %s\n" caption)
      (format "#+NAME: %s\n" name)
      (format "#+ATTR_LATEX: %s\n" attr-latex)
      (format "#+ATTR_HTML: %s\n" attr-html)
      (format "[[file:%s]]" image-file))))

(defun literef-subgraph-image-file-name(file-name)
  "If FILE-NAME is under the LiteRef directory, return the lisp expression that constructs the file name using `literef-directory'. Otherwise, return FILE-NAME as is in string quotes."
  (let* ((ignore-case)
	 (literef (expand-file-name literef-directory))
	 (file (expand-file-name file-name)))
    (if (string-prefix-p literef (file-name-directory file))
	(concat
	 "(concat literef-directory "
	 "\""
	 (substring file (length literef-directory))
	 "\""
	 ")")
      (concat "\"" file-name "\""))))

(defun literef-insert-subgraph-image-file-link()
  "The interactive interface for inserting the code block that calls `literef-subgraph-image-file-link'."
  (interactive)
  (let* ((key (literef-current-key))
	 (source-quotes "\"")
	 (source
	  (let* ((prompt
		  (concat
		   "Source file or key:    "
		   (if key
		       "Current key (c)  |  Other key (o)  | File (f)"
		     "Current file (c)  |  Other file (o)  | Key (k)")))
		 (choice (literef-read-char
			  prompt
			  (list ?c ?o (if key ?f ?k)))))
	    (cond
	     ((eq choice ?c) (if key key (buffer-file-name)))
	     ((or (and key (eq choice ?o)) (eq choice ?k))
	      (let (literef-helm-no-insert)
		(org-ref-insert-link nil)))
	     (t (progn
		  (setq source-quotes "")
		  (literef-subgraph-image-file-name
		   (read-file-name "Choose file: ")))))))
	 (file-node
	  (when (not (literef-key-exists source))
	    (y-or-n-p "Show file node?")))
	 (file-node-name
	  (when file-node
	    (read-string "File node name: "
			 (file-name-nondirectory source))))
	 (filter (literef-read-arc-filter "Arc filter: "))
	 (caption (read-string "#+CAPTION: "))
	 (name (read-string "#+NAME: "))
	 (latex-attrs
	  (read-string "#+ATTR_LATEX: "
		       literef-default-image-latex-attrs))
	 (html-attrs
	  (read-string "#+ATTR_HTML: "
		       literef-default-image-html-attrs)))
    (insert
     "#+BEGIN_SRC emacs-lisp :exports results :results raw\n"
     "(literef-subgraph-image-file-link "
     source-quotes source source-quotes " "
     (if file-node "t" "nil") " "
     (if file-node-name (concat "\"" file-node-name "\"") "nil") " "
     "\"" filter "\"" " " 
     "\"" caption "\"" " "
     "\"" name "\"" " " 
     "\"" latex-attrs "\"" " "
     "\"" html-attrs "\"" " " 
     ")\n"
     "#+END_SRC")))

(setq org-image-actual-width nil)
(setq org-latex-prefer-user-labels t)
(advice-add 'org-export-to-file :around #'literef-export-to-file)

(provide 'literef-export)
