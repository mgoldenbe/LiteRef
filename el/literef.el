(require 'bibtex-completion)
(require 'org-inlinetask)

(require 'literef-config)
(require 'literef-utils)
(require 'literef-helm)
(require 'literef-latex-map)
(require 'literef-annotations)
(require 'literef-graph)
(require 'literef-subgraph)
(require 'literef-export)
(require 'literef-pdf)
(require 'literef-server)

;; start the server, while making sure that
;; only one instance of it is runnning.
(shell-command "pkill literef-server")
(call-process-shell-command
 (concat (file-name-directory load-file-name)
	 "py/literef-server.py" " " literef-directory "&") nil 0)
	   
;; advice org-ref-helm-insert-cite-link to begin by re-reading the default bibliography,
;; since entries could be added/removed.
;; (advice-add 'org-ref-helm-insert-cite-link :before #'literef-set-default-bibliography)
(advice-add 'org-ref-helm-cite :before #'literef-set-default-bibliography)

;; override bibtex-completion-fallback-candidates to not offer adding an entry to each bib file.
(defun bibtex-completion-fallback-candidates ()
  "Overrides bibtex-completion-fallback-candidates to not offer adding an entry to each bib file."
  bibtex-completion-fallback-options)

;;;; BEGIN: Clicking citation link ------------------------------

(defun  literef-cite-onclick-function()
  "Handles following a citation link."
  (let (ans)
    (unwind-protect
	(progn
	  (org-ref-cancel-link-messages)
	  (setq ans (literef-read-char
		     (concat
		      "Choose action:  "
		      "  Notes (n)  |"
		      "  PDF (p)  |"
		      "  BibFile (b)")
		     '(?n ?b ?p))))
      (org-ref-show-link-messages))
    (when (eq ans ?n) (literef-open-notes))
    (when (eq ans ?p) (literef-open-pdf))
    (when (eq ans ?b) (literef-open-bibfile))))

(setq org-ref-cite-onclick-function
      (lambda(_key) (literef-cite-onclick-function)))
;;;; END --------------------------------------------------------

;;;; BEGIN: Copying and pasting key(s) --------------------------
(defun literef-copy-current-key()
  "Copy the current key for later intelligent yanking."
  (interactive)
  (let ((key (literef-current-key)))
    (when key (kill-new key))))

(defun test()
  (interactive)
  (insert-for-yank "aaa"))

(defun literef-insert-for-yank (orig-fun string)
  "The version of insert-for-yank that handles the keys being yanked intelligently.

The string to be yanked is preceeded by the prefix computed as follows:

1. If over a citation key, go to its end.

2. Split the contents being yanked based on commas and analyze the first entry. If it is not a valid key, the prefix is empty. Otherwise, proceed to step 2. 

3. If the point is right after 'cite:', the prefix is empty. Otherwise, proceed to step 3. 

4. If the point is over a key, the prefix is ','. Otherwise, the prefix is 'cite:'.

Once the original function is called, the current citation link (if the cursor is over one) is sorted subject to the value of `literef-sort-citation-links'. 
"
  (let ((link (literef-citation-link-under-cursor)))
    (when link
      (let ((link-end (literef-link-end link)))
	(when (> link-end (point))
	  (goto-char (literef-link-end link))))))
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
    (when literef-sort-citation-links (literef-sort-citation-link t))
    ))

(advice-add 'insert-for-yank :around #'literef-insert-for-yank)

(defun literef-bibtex-from-clipboard()
  "Creates an entry from the BibTeX code saved in the clipboard."
  (interactive)
  (with-temp-file (concat literef-drop-directory "temp.bib")
    (yank)))
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
	 (element (progn
		    (beginning-of-line)
		    (car (org-element-at-point))))
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
	      (if (eq element 'plain-list)
		  (progn
		    (goto-char save-point)
		    (unwind-protect
			(progn
			  (org-ref-cancel-link-messages)
			  (y-or-n-p "Insert inline tasks?"))
		      (org-ref-show-link-messages)))
		nil
		)))
	;; set the default todo keyword
	(setq org-inlinetask-default-state
	      (when org-todo-keywords-1 (car org-todo-keywords-1)))
	
	;; remove the original line
	(kill-region prefix-begin postfix-end)

	;; insert a line for each key
	(dolist (key keys nil)
	  ;; get title and authors
	  (let* ((title-author (and insert-title-author (concat (literef-key-string key) " "))))
	    (unless (string= key (car keys)) (insert "\n"))
	    (insert prefix (concat title-author) "cite:" key postfix))
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

;;;; BEGIN: Sorting keys and citation links ---------------------

(defun literef-sort-keys(keys criteria)
  "Sort keys according to CRITERIA, which can be either a string of characters as in `literef-char-to-compare' or a list as returned by `literef-criteria-list'."
  (let (res
	(literef-criteria
	 (if (stringp criteria)
	     (literef-criteria-list (split-string criteria ","))
	   criteria))
	candidates)
    ;; Form the list of candidates (with empty string representations)
    (dolist (key keys nil)
      (push (cons "" (bibtex-completion-get-entry key)) candidates))
    ;; Now sort it
    (setq candidates (-sort 'literef-compare candidates))
    ;; Now form the result
    (dolist (c candidates nil)
      (push (literef-candidate-property "=key=" c) res))
    (reverse res)))

(defun literef-sort-citation-link(&optional no-read-criteria criteria)
  "Sort the current citation. The sorting criteria criteria are read from the user. If the optional NO-READ-CRITERIA is set, `literef-citation-link-sorting-criteria' is used as the sorting criteria. In addition, if CRITERIA is set (in which case, NO-READ-CRITERIA should also be set), it is used as the sorting criteria."
  (interactive)
  (let ((link (literef-citation-link-under-cursor)))
    (when link
      (let* ((orig-keys (literef-link-path-components link))
	     (keys (literef-sort-keys 
		    orig-keys
		    (if no-read-criteria
			(if criteria
			    criteria
			  literef-citation-link-sorting-criteria)
		      (literef-read-sorting-criteria)))))
	(save-excursion
	  (goto-char (literef-link-begin link))
	  (re-search-forward (literef-link-path link))
	  (replace-match (literef-join-strings keys ",")))))))

(defun literef-sort-citation-links(&optional no-read-criteria)
  "Sort all citation links in the current buffer. The sorting criteria criteria are read from the user. If the optional NO-READ-CRITERIA is set, `literef-citation-link-sorting-criteria' is used as the sorting criteria."
  (interactive)
  (save-excursion
    (let ((criteria
	   (unless no-read-criteria (literef-read-sorting-criteria))))
      (dolist (link (literef-citation-links) nil)
	(goto-char (literef-link-end link))
	(literef-sort-citation-link t criteria)))))

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
