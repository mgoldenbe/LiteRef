(defun literef-follow-pdf-annotation-link(path)
  (let* ((parts (split-string path ":"))
	 (key (elt parts 0))
	 (annot-id (elt parts 1)))
    (literef-open-key-pdf key)
    (pdf-annot-show-annotation (pdf-annot-getannot annot-id) t)))

(org-link-set-parameters
 literef-pdf-annotation-link
 :follow 'literef-follow-pdf-annotation-link
 :export (lambda (path desc backend) "") ; ignore the link
 :face `(:foreground ,literef-pdf-annotation-color)
 :help-echo "Click to jump to the annotation in the PDF.")

(defun literef-buffers-in-mode(mode)
  "Get all buffers in the major mode MODE."
  (save-excursion
    (let ((res)
	  (orig-buffer (current-buffer)))
      (catch 'ok
	(while t
	  (other-window 1)
	  (when (eq major-mode mode)
	    (push (current-buffer) res))
	  (when (eq (current-buffer) orig-buffer)
	    (throw 'ok nil))))
      res)))

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
	(literef-open-key-pdf key)
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
		(pdf-annot-get-id (pdf-annot-read-annotation
				   "Click on the annotation to select it.")))))
      (set-mouse-pixel-position
       (car orig-mouse-position)
       (car (cdr orig-mouse-position))
       (cdr (cdr orig-mouse-position)))
      (if annot-id
	  (insert (concat "annot:" key ":" (symbol-name annot-id)))
	(message "An error occurred while selecting an annotation")))))


;;;; Getting BibTeX entry
(defun literef-get-region-bibtex()
  "Get BibTeX entry based on the text in the region."
  (interactive)
  (let ((query (car (pdf-view-active-region-text))))
    (literef-server-request "getBib" query)))

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
