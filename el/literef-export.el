(defun literef-key-notes-p(key)
  "Predicate showing whether the notes for KEY are to be inserted.

Currently it returns t if the notes file is of non-zero size."
  (let ((notes-file (literef-notes-filename key))
	(index-of-size-attribute 7))
    (> (elt (file-attributes notes-file) index-of-size-attribute) 0)))

(defun literef-reference-text(keys)
  "Compute text for the reference for the given KEYS."

  (let* ((keys ;; only the keys whos notes are exported.
	  (let (res)
	    (dolist (key keys nil)
	      (when (literef-key-notes-p key)	 
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

(defun literef-insert-note-references()
  "For each citation in the current buffer, insert a reference to the sections corresponding to the keys in the selected subgraph. Properly handle comma-separated citations."
  (let ((shift 0))
    (dolist (link (literef-citation-links) nil)
      (let* ((keys (literef-link-path-components link))
	     (reference (literef-reference-text keys)))
	(goto-char (+ (literef-link-end link) shift))
	(insert reference)
	(setq shift (+ shift (length reference)))))))

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
    (dolist (key (literef-hash-keys-to-list literef-subgraph) nil)
      (when (and (literef-key-exists key) (literef-key-notes-p key))
	(push key res)))
    (sort res 'string<)))
    
(defun literef-export-to-file(orig-fun &rest args)
  "The version of `org-export-to-file' that supports exporting the subgraph.

It performs some pre-processing and then calls the original `org-export-to-file'."
  (message "In literef-export-to-file")
  (let* ((current-subgraph literef-subgraph)
	 (source-type (literef-subgraph-source-property :source-type))
	 (source (literef-subgraph-source-property :source-name))
	 (buffer-name
	  (if (not (boundp 'literef-subgraph-export))
	      (buffer-name)
	    (if (eq source-type :buffer)
		(condition-case nil
		    (with-current-buffer source (buffer-name))
		  (error (error (concat "The buffer " source " does not exist anymore. Consider running literef-reset-subgraph-selection to reset the subgraph selection."))))
	      (read-string "Enter the name of export buffer: "))))
	 (buffer-string
	  (if (not (boundp 'literef-subgraph-export))
	      (buffer-string)
	    (when (eq source-type :buffer)
	      (with-current-buffer source (buffer-string))))))
    (with-temp-buffer
      (org-mode)
      
      ;; Name the buffer.
      (rename-buffer buffer-name t)
	
      ;; Insert contents of a buffer if necessary.
      (when buffer-string
	(when (boundp 'literef-subgraph-export)
	  (let* ((default-section-name
		   (literef-subgraph-source-property :buffer-node-name))
		 (section-name
		  (read-string
		   (concat "Enter the title of the buffer secion "
			   "(default: " default-section-name
			   ") or nil to not create a section: ")
		   nil nil default-section-name)))
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
		    "Should a section for notes be created?"))
		   (notes-stars (if create-notes-section "**" "*")))
	    (when create-notes-section (insert "* Notes\n"))
	    (dolist (key keys nil)
	      (let ((notes-file (literef-notes-filename key)))
		(insert notes-stars " " (literef-key-string key) "\n"
			"<<sec:" key ">>" "\n")
		(insert "#+INCLUDE: " notes-file "\n")))))))
		
      ;; Expand INCLUDEs
      (org-export-expand-include-keyword)
      (end-of-buffer)

      ;; Sort citation links
      (when literef-sort-citation-links (literef-sort-citation-links t))
      
      ;; Insert references to note sections.
      (when (boundp 'literef-subgraph-export)
	(literef-insert-note-references))
      (end-of-buffer)

      ;; Insert bibliography if needed.
      (when (> (length (literef-buffer-keys)) 0)
	(let* ((bib-file-name (concat buffer-name ".bib"))
	       (append-text (concat "\n" "bibliographystyle:"
				    literef-bibliography-style "\n"
				    "bibliography:" bib-file-name)))
	  (insert append-text)
	  (literef-make-bib-file bib-file-name)))

      (apply orig-fun args))))

(defun literef-subgraph-export-dispatch()
  "The version of `org-export-dispatch' that works with the selected subgraph."
  (interactive)
  (let ((literef-subgraph-export t))
    (org-export-dispatch)))

(advice-add 'org-export-to-file :around #'literef-export-to-file)

(provide 'literef-export)
