(require 'bibtex-completion)
(require 'org-inlinetask)

(require 'literef-config)
(require 'literef-utils)
(require 'literef-helm)
(require 'literef-latex-map)
(require 'literef-annotations)
(require 'literef-graph)
(require 'literef-pdf)

;; advice org-ref-helm-insert-cite-link to begin by re-reading the default bibliography,
;; since entries could be added/removed.
;; (advice-add 'org-ref-helm-insert-cite-link :before #'literef-set-default-bibliography)
(advice-add 'org-ref-helm-cite :before #'literef-set-default-bibliography)

;; override bibtex-completion-fallback-candidates to not offer adding an entry to each bib file.
(defun bibtex-completion-fallback-candidates ()
  "Overrides bibtex-completion-fallback-candidates to not offer adding an entry to each bib file."
  bibtex-completion-fallback-options)

; Open notes of the paper when its citation link is clicked. 
(setq org-ref-cite-onclick-function (lambda(_key) (literef-open-notes)))

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
(defun literef-key-string(key)
  "Compute a string consisting of title and author."
  (let* ((entry (bibtex-completion-get-entry key))
	 (authors
	  (literef-translate-latex
	   (bibtex-completion-apa-get-value "author" entry)))
	 (title
	  (literef-translate-latex
	   (bibtex-completion-get-value "title" entry))))
    (concat  "\"" title "\" by " authors)))

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
	     (postfix (buffer-substring postfix-begin postfix-end))
	     (inline-tasks-flag
	      (if (eq (char-after prefix-begin) ?*)
		  nil
		(progn
		  (goto-char save-point)
		  (unwind-protect
		      (progn
			(org-ref-cancel-link-messages)
			(y-or-n-p "Insert inline tasks?"))
		    (org-ref-show-link-messages)))
		  )))
	;; set the default todo keyword
	(setq org-inlinetask-default-state
	      (when org-todo-keywords-1 (car org-todo-keywords-1)))
	
	;; remove the original line
	(kill-region prefix-begin postfix-end)

	;; insert a line for each key
	(dolist (key keys nil)
	  ;; get title and authors
	  (let* ((title-author (and insert-title-author (literef-key-string key))))
	    (unless (string= key (car keys)) (insert "\n"))
	    (insert prefix (concat title-author) postfix " cite:" key))
	  (when inline-tasks-flag
	    (org-inlinetask-insert-task)
	    (search-forward "END")))))
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
