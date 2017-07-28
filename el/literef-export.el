;; The current solution works without modifying the export function
;; of the citation links.
;; Here is how to do it in case such modification is needed in the future:
;; (advice-add (org-link-get-parameter "cite" :export) :after #'literef-cite-export)

(setq literef-section-name-history nil)

(defun literef-number-or-nil-p(string)
  "Determines whether STRING is number or nil."
  (let ((converted (string-to-number string)))
    (if (= converted 0)
	(if (or (string= string "0") (string= string "nil"))  t nil)
      t)))

(defun literef-number-or-nil(string)
  "Convert STRING to number or nil. The string is assumed to be one of the two."
  (if (string= string "nil") nil (string-to-number string)))

(defun literef-read-number-or-nil(prompt default)
  "Read either number or nil from the user."
  (let ((res "error"))
    (while (not (literef-number-or-nil-p res))
      (setq res (read-string prompt nil nil default)))
    (floor (literef-number-or-nil res))))

(defun literef-export-notes-p()
  "Predicate that checks whether notes are to be exported."
  (not (and literef-export-depth (= literef-export-depth 0))))

;; Functions related to getting the list of links are based on this reply
;; https://emacs.stackexchange.com/a/16914/16048
(defun literef-is-citation-link(link)
  "Return t if the link is a citation and nil otherwise"
  (string= (substring (org-element-property :type link) 0 4) "cite"))

(defun literef-link-keys(link)
  "Extract keys from the link."
  (split-string (org-element-property :path link) ","))

(defun literef-link-path-keys(path)
  "Extract keys from the link path."
  (split-string path ","))

(defun literef-all-keys()
  "Compute the list of all keys cited in the current buffer"
  (let (res)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(when (literef-is-citation-link link)
	  (dolist (key (literef-link-keys link) nil)
	    (setq res (cons key res))))))
    res))

(defun literef-citation-link< (link1 link2)
  "Compare two citation links."
  (< (elt link1 1) (elt link2 1)))

(defun literef-citation-links()
  "Compute the list of all citation links with begin and end positions."
  (let (res)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(when (literef-is-citation-link link)
	  (let* ((path (org-element-property :path link))
		 (begin (org-element-property :begin link))
		 (end (org-element-property :end link))
		 (actual-end
		  (progn
		    (goto-char end)
		    (1+ (search-backward-regexp "[^[:space:]]")))))
	    (setq res (cons (list path begin actual-end) res))))))
    (sort (copy-seq res) #'literef-citation-link<)))

(defun literef-reference-text(link)
  "Compute text for the reference for the given LINK."

  (let* ((keys ;; only the keys whos notes are exported.
	  (let (res)
	    (dolist (key (literef-link-path-keys path) nil)
	      (when (and (literef-key-notes-p key) (member key parsed-keys))
		(setq res (cons key res))))
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
  "For each citation for which notes are exported, insert a note reference. Properly handle comma-separated citations."
  (interactive)
  (let ((shift 0))
    (dolist (link (literef-citation-links) nil)
      (let* ((path (elt link 0))
	     (end (elt link 2))
	     (reference (literef-reference-text path)))
	(goto-char (+ end shift))
	(insert reference)
	(setq shift (+ shift (length reference))))))
  nil)
    
;; Source: http://ergoemacs.org/emacs/elisp_hash_table.html
(defsubst hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) hash-table)
    keys))

(defun literef-add-to-next-iter(key)
  "Perform duplicate detection on KEY and then add it to the list of exported keys and to the list of keys for the next iteration."
  (unless (or
	   (gethash key cur-iter-hash nil)
	   (gethash key next-iter-hash nil)
	   (gethash key exported-keys nil))
    (setq next-iter-list (cons key next-iter-list))
    (puthash key t next-iter-hash)
    (puthash key t exported-keys)))

(defun literef-exported-keys()
  "Performs uniform-cost search to compute the list of exported keys and the list of keys whose notes have been parsed.

The open lists for the current and the next depth are maintained in a combination of a regular list and a hash table. 'exported-keys is the closed list. 
"
  (let ((depth 0)
	(cur-iter-list) (cur-iter-hash (make-hash-table))
	(next-iter-list) (next-iter-hash (make-hash-table))
	(exported-keys (make-hash-table))
	parsed-keys)
    (dolist (key (literef-all-keys) nil)
      (literef-add-to-next-iter key))
    (while ;; there are keys for next iteration depth not exceeded
	(and
	 next-iter-list
	 (not (and literef-export-depth
		   (> (setq depth (1+ depth)) literef-export-depth))))
      (setq cur-iter-list next-iter-list)
      (setq cur-iter-hash next-iter-hash)
      (setq next-iter-list nil)
      (setq next-iter-hash (make-hash-table))
      (while cur-iter-list
	(let* ((key (car cur-iter-list)))
	  (remhash key cur-iter-hash)
	  (setq cur-iter-list (cdr cur-iter-list))
	  (when (literef-key-notes-p key)
	    (setq parsed-keys (cons key parsed-keys))
	    (dolist (key
		     (with-temp-buffer
		       (insert-file-contents (literef-notes-filename key))
		       (literef-all-keys))
		     nil)
	      (literef-add-to-next-iter key))))))
    (list (hash-table-keys exported-keys) parsed-keys)))

;; Source: https://emacs.stackexchange.com/a/31763/16048
(defmacro with-cloned-buffer(&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let ((buffer-file-name nil))
       (clone-buffer nil t)
       (let ((,return-value (progn ,@body)))
	 (kill-buffer-and-window)
	 ,return-value))))

(defun literef-forward-search-stars()
  "Find stars forward in the buffer."
  (interactive)
  (re-search-forward "[*]+" nil t)
  (match-string 0))

(defun literef-make-bib-file(bib-file)
  "Make the bibliography file containing only the entries for the used keys."
  (when (file-exists-p bib-file) (delete-file bib-file))
  (with-temp-file bib-file
    (dolist (key (delete-dups (sort exported-keys 'string<)) nil)
      (let ((cur-bib (literef-bib-filename key)))
	(and
	 (file-exists-p cur-bib)
	 (insert-file-contents cur-bib)))
      (goto-char (point-max))
      (insert "\n"))))

(defun literef-key-notes-p(key)
  "Predicate showing whether the notes for a particular key are to be exported."
  (let ((notes-file (literef-notes-filename key))
		  (index-of-size-attribute 7))
    (> (elt (file-attributes notes-file) index-of-size-attribute) 0)))

(defun literef-export-to-file-cloned-buffer()
  "The actions to be performed in the cloned buffer before applying the original org-export-to-file."
  ;; First, delete everything besides the narrowed region.
  (delete-region narrow-end (point-max))
  (delete-region (point-min) narrow-beg)
  ;; Expand the include keywords.
  (org-export-expand-include-keyword)
  ;; Now put everything under section if notes are exported.
  (when (stringp section-name)
    (let* ((first-stars (progn (goto-char (point-min)) (literef-forward-search-stars)))
	   (new-heading-stars (if (string= first-stars "") "*" first-stars))
	   (section-text (concat new-heading-stars " " section-name "\n")))
      (goto-char (point-min))
      (set-mark (point-max))
      (org-do-demote)
      (deactivate-mark)
      (goto-char (point-min))
      (insert section-text)))
  
  ;; Up to here we have cared for the current buffer.
  (let* ((exported-keys-pair (literef-exported-keys))
	 (exported-keys (elt exported-keys-pair 0))
	 (parsed-keys (elt exported-keys-pair 1))
	 (bib-file (concat buffer-name ".bib"))
	 (append-text (concat "\n" "bibliographystyle:"
			      literef-bibliography-style "\n"
			      "bibliography:" bib-file)))
    (goto-char (point-max))
    ;; Insert the notes
    (when (> (length parsed-keys) 0) (insert "* Notes\n")
	  (dolist (key (delete-dups (sort parsed-keys 'string<)) nil)
	    (let ((notes-file (literef-notes-filename key)))
	      (insert "** " (litered-key-string key) "\n" "<<sec:" key ">>" "\n")
	      (insert "#+INCLUDE: " notes-file "\n")))
	  ;; Insert references to notes
	  (literef-insert-note-references))
    ;; Insert bibliography and create it.
    (goto-char (point-max))
    (insert append-text)
    (literef-make-bib-file bib-file)
    ;; (message "Buffer contents:\n%s" (buffer-substring (point-min) (point-max)))
    ))
  
(defun literef-export-to-file(orig-fun type file &rest args)
  "Export to latex that creates and adds bibliography."
  (let ((literef-export-depth (literef-read-number-or-nil "Enter the depth for notes export [0] (for no limit, enter nil): " "0")))
    (save-window-excursion
      (let* ((buffer-name (file-name-base file))
	     (default-section-name
	       (if literef-section-name-history
		   (car literef-section-name-history)
		 (capitalize buffer-name)))
	     (section-name
	      (if (literef-export-notes-p)
		  (let ((answer
			 (read-string
			  (concat "The main section name "
				  "(nil for no section) [" 
				  default-section-name
				  "]: ")
			  nil
			  'literef-section-name-history
			  default-section-name
			  )))
		    (if (string= answer "nil") nil answer))
		nil))
	     (narrow-beg (point-min))
	     (narrow-end (point-max)))
	(with-cloned-buffer
	  (literef-export-to-file-cloned-buffer)
	  (apply orig-fun (cons type (cons file args))))))))

(advice-add 'org-export-to-file :around #'literef-export-to-file)

(provide 'literef-export)
