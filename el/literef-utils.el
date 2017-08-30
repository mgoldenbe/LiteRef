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

;; Source: https://emacs.stackexchange.com/a/34665/16048
(defun literef-replace-in-string-whole-words(what with in)
  "Like 'replace-in-string, but replaces whole words."
  (replace-regexp-in-string (concat "\\b" what "\\b")  with in))

(defun literef-join-strings(strings separator)
  "Join the list STRINGS of strings putting the SEPARATOR string between them."
  (let ((remain (length strings))
	(res ""))
    (dolist (s strings res)
      (setq res (concat res s))
      (setq remain (1- remain))
      (when (> remain 0) (setq res (concat res separator))))))

;; Source: https://emacs.stackexchange.com/a/19878/16048
(defun literef-eval-string (string)
  "Evaluate elisp code stored in a string."
  (eval (car (read-from-string string))))

;; Based on: http://ergoemacs.org/emacs/elisp_hash_table.html
(defun literef-hash-keys-to-list (hash)
  "Return a list of keys in HASH."
  (let (res)
    (maphash (lambda (k _v) (push k res)) hash)
    res))

(defun literef-hash-pairs-to-list (hash)
  "Return a list of keys in HASH."
  (let (res)
    (maphash (lambda (k v) (push (list k v) res)) hash)
    res))

(defun literef-link-type(link)
  "The type of the LINK."
  (org-element-property :type link))

(defun literef-link-begin(link)
  "The beginning of the LINK."
  (org-element-property :begin link))

(defun literef-link-end(link)
  "The actual end of the LINK without spaces after it."
  (save-excursion
    (goto-char (org-element-property :end link))
    (1+ (search-backward-regexp "[^[:space:]]"))))

(defun literef-link-path(link)
  "The path in the LINK."
  (org-element-property :path link))

(defun literef-backward-adjacent-org-element(link)
  "The org-element adjacent and before the given LINK."
  (save-excursion
    (goto-char (literef-link-begin link))
    (let ((pos (search-backward-regexp "[^[:space:]]")))
      (if pos (org-element-context) nil))))

(defun literef-link-path-components(link)
  "Extract keys from the link path."
  (split-string (literef-link-path link) ","))

(defun literef-citation-link< (link1 link2)
  "Compare two citation links."
  (< (literef-link-begin link1) (literef-link-begin link2)))

(defun literef-all-links(predicate)
  "Compute the list of all citation links in the current buffer that satisfy a given PREDICATE (if PREDICATE is nil, all links are included). The links are sorted by the begin position." 
  (let (res)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(when (or (not predicate) (funcall predicate link))
	  (setq res (cons link res)))))
    (sort (copy-seq res) #'literef-citation-link<)))

(defun literef-citation-link-p(link)
  "Return t if the link is a citation and nil otherwise"
  (let ((type (literef-link-type link)))
    (and type (>= (length type) 4) (string= (substring type 0 4) "cite"))))

(defun literef-annotation-link-p(link)
  "Return t if the link is a citation annotation and nil otherwise"
  (string= (literef-link-type link) literef-annotation-link))

(defun literef-citation-links()
  "Compute the list of all citation links in the current buffer, sorted by the begin position."
  (literef-all-links #'literef-citation-link-p))

(defun literef-annotation-links()
  "Compute the list of all annotation links in the current buffer, sorted by the begin position."
  (literef-all-links #'literef-annotation-link-p))

(defun literef-buffer-keys()
  "Compute the list of all keys cited in the current buffer, sorted and with duplicates removed."
  (let (res)
    (dolist (link (literef-citation-links) nil)
      (dolist (key (literef-link-path-components link) nil)
	(setq res (cons key res))))
    (delete-dups (sort res 'string<))))

(defun literef-all-keys()
  "Compute the list of all keys."
  (let (res)
    (dolist (key (directory-files literef-papers-directory) nil)
      (when (literef-key-exists key) (push key res)))
    (sort res 'string<)))

(defun literef-bib-files (&optional _arg)
  "Compute the list of bib files."
  (sort (file-expand-wildcards
	 (concat literef-papers-directory "*/paper.bib")) 'string<))

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

(defun literef-creation-timestamp(key)
  "Compute the creation timestamp of the KEY as a floating-point number of seconds. It uses the modification timestamp of the bib file, since, in general, the creation timestamp might not be stored by the operating system and accessing the creation date for ext4 is not trivial."
  (let* ((bibfile-name (literef-bib-filename key))
	 (attribute (elt (file-attributes bibfile-name) 5)))
    (time-to-seconds attribute))) 

(defun literef-notes-filename(key)
  "Compute name of the notes file based on the key and the extension"
  (literef-filename key "org"))

(defun literef-pdf-filename(key)
  "Compute name of the pdf file based on the key and the extension"
  (literef-filename key "pdf"))

(defun literef-request-filename()
  "Compute name of the request file"
  (concat literef-drop-directory "request." (number-to-string (float-time)) ".rqt"))

(defun literef-open-key-notes(key)
  "Open notes for KEY."
  (let ((filename (literef-notes-filename key)))
    (find-file-other-window filename)))

(defun literef-open-notes()
  "Open notes for the cite link under cursor"
  (let ((key (org-ref-get-bibtex-key-under-cursor)))
    (literef-open-key-notes key)))

(defun literef-open-key-pdf(key)
  "Open the pdf for KEY."
  (let ((filename (literef-pdf-filename key)))
    (if (file-exists-p filename)
	(shell-command (concat literef-pdf-viewer " " filename))
      (with-temp-file (literef-request-filename)
	(message "The PDF is not found. Sending query to the daemon (make it's running!)")
	(run-with-timer 3 nil (lambda () (message nil)))
	(insert (concat "getPdf" " " key))))))

(defun literef-open-pdf()
  "Open the pdf for the citation under cursor or for the paper of the current notes file."
  (interactive)
  (let ((key (literef-current-key)))
    (when key
      (literef-open-key-pdf key))))

(defun literef-current-folder-key()
  "Compute key based on the folder in which the file being visited is located.

If there the visited folder does not correspond to a key, returns nil."
  (let ((key (car (last (nbutlast (split-string (file-name-directory buffer-file-name) "/"))))))
    (if (file-directory-p (concat literef-papers-directory key))
	key
      nil)))

(provide 'literef-utils)
