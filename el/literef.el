(require 'org)

(setq org-ref-completion-library 'org-ref-helm-cite)
(require 'org-ref)

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

;; Turn off code evaluation security.
(setq org-confirm-babel-evaluate nil)

;; Set up pdflatex compilation.
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))

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
		      "  BibFile (b)  |"
		      "  Citation sub-graph (s)")
		     '(?n ?p ?b ?s))))
      (org-ref-show-link-messages))
    (when (eq ans ?n) (literef-open-notes))
    (when (eq ans ?p) (literef-open-pdf))
    (when (eq ans ?b) (literef-open-bibfile))
    (when (eq ans ?s) (literef-select-subgraph))))

(setq org-ref-cite-onclick-function
      (lambda(_key) (literef-cite-onclick-function)))
;;;; END --------------------------------------------------------

;;;; BEGIN: Copying and pasting key(s) --------------------------
(defun literef-copy-current-key()
  "Copy the current key for later intelligent yanking."
  (interactive)
  (let ((key (literef-current-key)))
    (when key (kill-new key))))

(defun literef-after-string-p(string)
  "Returns t if point is after STRING and nil otherwise."
  (and (> (point) (length string))
       (equal
	(buffer-substring (- (point) (length string)) (point))
	string)))

(defun literef-after-citation-type-p()
  "Returns t if point is after citation type such as citeauthor: and nil otherwise."
  (let (res)
    (dolist (type org-ref-cite-types res)
      (setq res (or res (literef-after-string-p (concat type ":")))))))

(defun literef-insert-for-yank (orig-fun string)
  "The version of insert-for-yank that handles the keys being yanked intelligently.

The string to be yanked is preceeded by the prefix computed as follows:

1. Split the contents being yanked based on commas and analyze the first entry. If it is not a valid key, the prefix is empty. Otherwise, proceed to step 2. 

2. If over a citation key, go to its end and set the prefix to be comma.

3. Otherwise, if right after comma that follows citation key or after citation type, the prefix is empty. 

4. Otherwise, the prefix is 'cite:'.

Once the original function is called, the current citation link (if the cursor is over one) is sorted subject to the value of `literef-sort-citation-links'. 
"
  (let* ((key (car (split-string string ",")))
	 (link (literef-citation-link-under-cursor))
	 (prefix
	  (cond
	   ((not (literef-key-exists key)) "")
	   (link
	    (let ((link-end (literef-link-end link)))
	      (goto-char link-end)
	      ","))
	   ((when (literef-after-string-p ",")
	      (save-excursion
		(goto-char (1- (point)))
		(literef-citation-link-under-cursor)))
	    "")
	   ((literef-after-citation-type-p) "")
	   (t "cite:"))))
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
	 (element (progn
		    (beginning-of-line)
		    (car (org-element-at-point))))
	 (line-end (progn (end-of-line) (point)))
	 (line-begin (progn (beginning-of-line) (point)))
	 (link (literef-first-citation-link-on-line)))
    (unless link
      (goto-char save-point)
      (error (error "No citation on this line")))
    (let* ((link-type (literef-link-type link))
	   (link-begin (literef-link-begin link))
	   (link-end (literef-link-end link))
	   (keys (literef-link-path-components link))
	   (prefix (buffer-substring line-begin link-begin))
	   (postfix (buffer-substring link-end line-end))
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
      (kill-region line-begin line-end)

      ;; insert a line for each key
      (dolist (key keys nil)
	;; get title and authors
	(let* ((title-author (and insert-title-author (concat (literef-key-string key) " "))))
	  (unless (string= key (car keys)) (insert "\n"))
	  (insert prefix (concat title-author) link-type ":" key postfix))
	  (when inline-tasks-flag
	    (org-inlinetask-insert-task)
	    (search-forward "END")))))
    (goto-char save-point))

(defun test()
  (interactive)
  (goto-char (literef-link-begin (literef-first-citation-link-on-line))))

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
	 (temp (shell-command-to-string command))
	 (raw-output
	  (if (> (length temp) 0)
	      (substring temp 0 -1)
	    ""))
	 (output (split-string raw-output "\n")))
    (when (eq (length (car output)) 0)
      (setq output nil)
      (message "No matches found."))
    (dolist (line output nil)
      (let* ((prefix-length (length literef-papers-directory))
	     (line-no-prefix (substring line prefix-length))
	     (key (car (split-string line-no-prefix "/"))))
	(insert-for-yank key)))))
;;;; END --------------------------------------------------------
