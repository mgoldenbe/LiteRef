;;; literef-pdf.el --- PDF annotation links, import of papers referenced in a PDF, searching in PDFs.

;; Copyright(C) 2017-2018 Meir Goldenberg

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module contains the definition of the PDF annotation link. It
;; provides functions for inserting such links into the researcher's
;; notes and handling the operations associated with these links,
;; including the translation of these links to page references during
;; export. In addition, this module handles import of papers by
;; letting the user select a region in the PDF, e.g. in the references
;; section of a paper. Lastly, the module supports searching for keys
;; whose PDF matches a search criteria and inserting those keys into
;; the current notes buffer.

;;; Code:
(pdf-tools-install)

(defun literef-follow-pdf-annotation-link(path)
  "Jump to the place in the PDF pointed to by PATH, which is the path component of a PDF annotation link."
  (let* ((parts (split-string path ":"))
	 (key (elt parts 0))
	 (annot-id (elt parts 1)))
    (switch-to-buffer (literef-open-key-pdf-raw key))
    (pdf-info-getannots)
    (pdf-annot-show-annotation (pdf-info-getannot annot-id) t)))

(defun literef-pdf-annotation-string(path &rest _args)
  "Compute the page reference string that is to be substituted during exporting the
survey for PDF annotation link with the path component PATH."
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
  "Compute the list of buffers that are currently in the major mode MODE."
  (let (res)
    (dolist (b (buffer-list) res)
      (with-current-buffer b
	(when (eq major-mode mode)
	  (push (current-buffer) res))))))

(defun literef-pdf-buffer-keys()
  "Return the list of keys, whose corresponding PDF is currently open
in a buffer. This is done by looping through the open buffers and
checking for each buffer currently in the PDFView mode whether it
corresponds to an entry in the papers database."
  (let (res)
    (dolist (buffer (literef-buffers-in-mode 'pdf-view-mode) res)
      (let ((key (literef-buffer-key buffer)))
	(when key (push key res))))))

(defun literef-cite-pdf-annotation()
  "Let the user choose an annotation in an open PDF of a paper by
clicking on the annotation. Inserts a PDF annotation link
corresponding to the chosen annotation into the active notes
buffer. Only PDFs open in the PDFView mode are handled."
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

;;;; Search based on the selected region in the PDF.

(defun literef-first-word(line)
  "Return the first word in the string LINE."
  (string-match "\\(^[[:alpha:]]+\\)" line)
  (match-string 1 line))

(defun literef-last-word-hyphen(line)
  "Return the last word in the string LINE if it is followed by
hyphen. If LINE is not ended with word and hyphen, return nil."
  (string-match "\\([[:alpha:]]+\\)-$" line)
  (match-string 1 line))

(defun literef-glue-p(line1-last line2-first)
  "Determine whether a line ending with the word LINE1-LAST and
hyphen and the line beginning with the word LINE2-FIRST should be
glued without the use of a hyphen when presenting those two lines as a
single line. This involves trying to glue them without a hyphen and
checking whether the resulting string is a word in the dictionary. If
both glued and non-glued variants are correct or wrong, then the user
is prompted to make a choice."
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
  "Compute a single-line query based on the possibly multi-line QUERY
for searching for a BibTeX entry in the online sources, while
correctly handling hyphen at the end of lines."
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

;;;; Searching for a BibTeX entry in the online sources.
(defun literef-get-region-bibtex()
  "Search for a BibTeX entry in the online sources based on the
selected region in the paper PDF."
  (interactive)
  (let ((query
	 (if (pdf-view-active-region-p)
	     (car (pdf-view-active-region-text))
	   (buffer-substring (region-beginning) (region-end)))))
    (literef-server-request
     "getBib" (literef-single-line-query query))))

;; (defun flatten-string-with-links (string)
;;     (replace-regexp-in-string "\\[\\[[a-zA-Z:%@/\.]+\\]\\[[a-zA-Z:%@/\.]+\\]\\]"
;;                 (lambda (s) (save-match-data
;; 			      (nth 2 (split-string s "[\]\[]+")))) string))

;; (flatten-string-with-links "[[abc][cdh]] [[a][b]]")

;;;; For a possible future feature.
(setq org-startup-with-inline-images t)

;; Source: https://stackoverflow.com/a/17438212/2725810
;; (defun my-org-screenshot ()
;;   "Take a screenshot into a timestamped uniquely named file in the
;; same directory as the org-buffer and insert a link to this file."
;;   (interactive)
;;   (setq filename
;;         (concat
;;          (make-temp-name
;;           (concat (buffer-file-name)
;;                   "_"
;;                   (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;   (call-process "import" nil nil nil filename)
;;   (insert (concat "[[" filename "]]"))
;;   (org-display-inline-images))

;;;; BEGIN: Searches --------------------------------------------
(defun literef-search-pdfs(string)
  "Yank into the current notes buffer the citation links for the keys
matched by searching for STRING in the first page of the PDFs in the
papers database. LiteRef's intelligent yanking is used. The function
relies on the `pdfgrep' shell command. The string STRING must be a
valid search pattern for that command."
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

(provide 'literef-pdf)
