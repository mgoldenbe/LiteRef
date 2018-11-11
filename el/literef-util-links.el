;;; literef-util-links.el --- utility functions for working with links.

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
;; This module contains some helper functions to make working with
;; links in the other modules convenient.

;;; Code:
(defun literef-link-type(link)
  "Return the type of the LINK."
  (org-element-property :type link))

(defun literef-link-begin(link)
  "The beginning point of the LINK."
  (org-element-property :begin link))

(defun literef-link-end(link)
  "The end point of the LINK. The spaces following the link are not included."
  (let ((pos (org-element-property :end link))) 
    (save-excursion
      (goto-char pos)
      (1+ (search-backward-regexp "[^[:space:]]")))))

(defun literef-link-path(link)
  "Return the path component of the LINK."
  (org-element-property :path link))

(defun literef-link-prev-non-space(link)
  "Return the position of the first non-blank character before the LINK."
  (save-excursion
    (goto-char (literef-link-begin link))
    (search-backward-regexp "[^[:space:]]")))

(defun literef-link-prev-element(link)
  "The org-element adjacent and before the LINK."
  (save-excursion
    (let ((pos (literef-link-prev-non-space link)))
      (when pos
	(goto-char pos)
	(org-element-context)))))

(defun literef-link-path-components(link)
  "Extract keys from the LINK's path component."
  (split-string (literef-link-path link) ","))

(defun literef-link< (link1 link2)
  "Determine which of the links LINK1 and LINK2 appears earlier in the buffer. The two links are assumed to appear in the same buffer. Return t if LINK1 appears before LINK2 and nil otherwise."
  (< (literef-link-begin link1) (literef-link-begin link2)))

(defun literef-all-links(predicate)
  "Compute the list of all links in the current buffer that satisfy a
given PREDICATE (if PREDICATE is nil, all links are included). The
links are sorted by their begin positions. The `:end' property is
substituted to be the actual end point of the link without spaces
after it."
  (let (res)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(when (or (not predicate) (funcall predicate link))
	  (setq res (cons link res)))))
    (sort (copy-seq res) #'literef-link<)))

(defun literef-citation-link-p(link)
  "Return t if the LINK is a citation link and nil otherwise."
  (let ((type (literef-link-type link)))
    (when (member type org-ref-cite-types) t)))

(defun literef-citation-function-link-p(link)
  "Return t if the LINK is a citation function link and nil otherwise."
  (string= (literef-link-type link) literef-citation-function-link))

(defun literef-citation-link-under-cursor()
  "Return the citation link at point. If the cursor is not over a
citation link, return nil."
  (ignore-errors
    (let ((object (org-element-context)))
      (when (and (literef-citation-link-p object)
		 (<= (point) (literef-link-end object)))
	object))))

(defun literef-first-citation-link-on-line()
  "Return the first citation link on the current line. If there is no citation link on the line, return nil."
  ;; A very inefficient bruteforce approach, which works and is fast enough.
  (save-excursion
    (let* ((res)
	   (begin (progn (beginning-of-line) (point)))
	   (end (progn (end-of-line) (point))))
      (goto-char begin)
      (while (and (not res) (< (point) end))
	(setq res (literef-citation-link-under-cursor))
	(goto-char (1+ (point))))
      res)))

(defun literef-citation-links()
  "Compute the list of all citation links in the current buffer, sorted by their begin positions."
  (literef-all-links #'literef-citation-link-p))

(defun literef-citation-function-links()
  "Compute the list of all citation function links in the current buffer, sorted by the begin position."
  (literef-all-links #'literef-citation-function-link-p))

(provide 'literef-util-links)
