(pdf-tools-install)

(defun literef-follow-pdf-annotation-link(path)
  (let* ((parts (split-string path ":"))
	 (key (elt parts 0))
	 (annot-id (elt parts 1)))
    (switch-to-buffer (literef-open-key-pdf-raw key))
    (pdf-info-getannots)
    (pdf-annot-show-annotation (pdf-info-getannot annot-id) t)))

(defun literef-pdf-annotation-string(path &rest _args)
  (let* ((parts (split-string path ":"))
	 (link (elt parts 1))
	 (link-parts (split-string link "-"))
	 (page (elt link-parts 1)))
    (concat "page " page)))
	 
(org-link-set-parameters
 literef-pdf-annotation-link
 :follow 'literef-follow-pdf-annotation-link
 :export 'literef-pdf-annotation-string
 :face `(:foreground ,literef-pdf-annotation-color)
 :help-echo "Click to jump to the annotation in the PDF.")

(defun literef-buffers-in-mode(mode)
  "Get all buffers in the major mode MODE."
  (let (res)
    (dolist (b (buffer-list) res)
      (with-current-buffer b
	(when (eq major-mode mode)
	  (push (current-buffer) res))))))

(defun literef-pdf-buffer-keys()
  "For each buffer in PDFView mode that corresponds to an entry, return the entry's key"
  (let (res)
    (dolist (buffer (literef-buffers-in-mode 'pdf-view-mode) res)
      (let ((key (literef-buffer-key buffer)))
	(when key (push key res))))))

(defun literef-cite-pdf-annotation()
  "Choose an annotation in an open paper PDF and cite it. Only buffers in PDFView mode are considered."
  (interactive)
  (let (annot-id
	(orig-mouse-position (mouse-pixel-position)))
    (let* ((keys (remove-duplicates (literef-pdf-buffer-keys)))
	   (key
	    (if (eq (length keys) 1)
		(car keys)
	      (ido-completing-read
	       "PDFs for the following keys are open. Please choose:\n"
	       keys))))
      (save-excursion
	(literef-open-key-pdf-raw key)
	;; (yes-or-no-p (prin1-to-string (frame-selected-window)))
	(let* ((window (window-inside-pixel-edges))
	       (left (elt window 0))
	       (top (elt window 1))
	       (right (elt window 2))
	       (bottom (elt window 3)))
	  (set-mouse-pixel-position
	   (selected-frame)
	   (/ (+ left right) 2)
	   (/ (+ top bottom) 2)))
	(setq annot-id
	      (ignore-errors
		(pdf-annot-get-id
		 (pdf-annot-read-annotation
		  "Click on the annotation to select it.")))))
      (set-mouse-pixel-position
       (car orig-mouse-position)
       (car (cdr orig-mouse-position))
       (cdr (cdr orig-mouse-position)))
      (if annot-id
	  (let ((string (concat "annot:" key ":"
				(symbol-name annot-id))))
	    (if buffer-read-only
		(progn
		  (kill-new string)
		  (message "The annotation link is in the kill ring."))
	      (insert string)))
	(message "An error occurred while selecting an annotation")))))

(defun literef-first-word(line)
  "Return the first word in LINE."
  (string-match "\\(^[[:alpha:]]+\\)" line)
  (match-string 1 line))

(defun literef-last-word-hyphen(line)
  "Return the last word in LINE. That word is assumed to be followed by hyphen. If the line is not ended with word and hyphen, return nil."
  (string-match "\\([[:alpha:]]+\\)-$" line)
  (match-string 1 line))

(defun literef-glue-p(line1-last line2-first)
  "Determines whether a line ending with the word LINE1-LAST and hyphen and the line beginning with the word LINE2-FIRST should be glued without the use of a hyphen when presenting those two lines as a single line."
  (let ((glued-correct
	 (literef-word-correct-p (concat line1-last line2-first)))
	(hyphened-correct
	 (and (literef-word-correct-p line1-last)
	      (literef-word-correct-p line2-first))))
    (cond
     ((and glued-correct (not hyphened-correct)) t)
     ((and hyphened-correct (not glued-correct)) nil)
     (t (y-or-n-p
	 (concat "Should the words " line1-last " and " line2-first
		 " be glued in the single-line version?"))))))

(defun literef-single-line-query(query)
  "Return a single-line query corresponding to QUERY, while correctly handling hyphen at the end of lines."
  (let* ((lines (split-string query "\n"))
	 (res (car lines))
	 (lines (cdr lines)))
    (dolist (line lines res)
      (let ((res-last (literef-last-word-hyphen res))
	    (line-first (literef-first-word line)))
	(if (and res-last line-first)
	    (progn
	      (when (literef-glue-p res-last line-first)
		(setq res (substring res 0 -1))) ;; remove the hyphen
	      (setq res (concat res line)))
	  ;; Neither glue nor hyphen.
	  (setq res (concat res " " line)))))))

;;;; Getting BibTeX entry
(defun literef-get-region-bibtex()
  "Get BibTeX entry based on the text in the region."
  (interactive)
  (let ((query
	 (if (pdf-view-active-region-p)
	     (car (pdf-view-active-region-text))
	   (buffer-substring (region-beginning) (region-end)))))
    (literef-server-request
     "getBib" (literef-single-line-query query))))

(defun flatten-string-with-links (string)
    (replace-regexp-in-string "\\[\\[[a-zA-Z:%@/\.]+\\]\\[[a-zA-Z:%@/\.]+\\]\\]"
                (lambda (s) (save-match-data
			      (nth 2 (split-string s "[\]\[]+")))) string))

(flatten-string-with-links "[[abc][cdh]]")

;;;; For a possible future feature.
(setq org-startup-with-inline-images t)

;; Source: https://stackoverflow.com/a/17438212/2725810
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(provide 'literef-pdf)
