;;; literef-helm.el --- the `helm' interface for searching in the papers database.

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
;; This module contains functions that implement the formation of the
;; candidate list of the `helm' interface for searching in the papers
;; database and handle actions associated with these candidates.

;;; Code:
(require 'helm)
(require 'cl)
(require 'dash)

;; Make sure all candidates are displayed
(setq helm-candidate-number-limit 1000000)

(defun literef-assoc(property a-list)
  "Return the value in A-LIST (see `https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html') associated with the PROPERTY of a candidate in `helm'. If no value is associated with the PROPERTY, return nil."
  (let ((raw (assoc property a-list)))
    (if raw (cdr raw) nil)))

(defun literef-candidate-property(property c)
  "Return the PROPERTY of the candidate C or nil if the candidate does not have such a property."
  (let ((fields (cdr c)))
    (literef-assoc property fields)))

(defun literef-candidate-venue(c)
  "Return the venue of the candidate C or nil of this property is not specified."
  (or (literef-candidate-property "booktitle" c)
      (literef-candidate-property "journal" c)))

(defun literef-candidate-venue-type(c)
  "Compute the venue type of the candidate C based on its key property."
  (let ((key (literef-candidate-property "=key=" c)))
    (string-match "[[:digit:]]\\{4\\}\\([^-]*\\)" key)
    (match-string 1 key)))

;;;; Changing the candidate strings.

(defun literef-candidate-helm-string(c)
  "Compute the string to be displayed in helm for the candidate C."
  (let* ((res "")
	 (fields (cdr c))
	 (key (literef-assoc "=key=" fields))
	 (creation-date (format-time-string
			 "%d-%m-%Y %H:%M:%S"
			 (seconds-to-time (literef-creation-timestamp key))))
	 (author
	  (literef-translate-latex (literef-assoc "author" fields)))
	 (title
	  (literef-translate-latex (literef-assoc "title" fields)))
	 (venue (literef-translate-latex (literef-candidate-venue c))))
    ;; (setq key (concat "[" key "]"))
    (setq key
    	  (propertize
    	   (concat "|" key "|") 'font-lock-face '(:foreground "blue")))
    (setq res (concat res key " (Created: " creation-date ")"))
    (dolist (field '(author title venue))
      (let ((value (symbol-value field)))
	(when value
	  ;; (let ((prefix (concat (capitalize (symbol-name field)) ": ")))
	  (let ((prefix (propertize
	  		 (concat (capitalize (symbol-name field)) ": ")
	  		 'font-lock-face '(:foreground "green"))))
	    (setq res (concat res "\n" prefix value))))))
    res))

(defun literef-candidate-transformer(candidates)
  "Transform the initial candidate list given by CANDIDATES."
  
  (let (res)
    (dolist (c candidates res)
      (let ((key (literef-candidate-property "=key=" c)))
	(when (or (not (boundp 'literef-subgraph-helm))
		  (gethash key (literef-subgraph-keys) nil))
	  (push (cons (literef-candidate-helm-string c) (cdr c)) res))))
    (setq res (reverse res))
    res
    ))

(push '(name . "Literef Helm") org-ref-helm-cite-source)

;; Filtering wouldn't work correctly unless
;; candidates are transformed at the beginning.
(push '(candidate-transformer . literef-candidate-transformer)
      org-ref-helm-cite-source)

;;;; Sorting

(defun literef-timestamp-compare(c1 c2)
  "Compare the candidates C1 and C2 based on timestamps of the corresponding entries in the papers database. Respect the value of the variable `literef-equal-timestamps' when deciding whether the two timestamps are equal."
  (catch 'ok
    (let* ((key1 (literef-candidate-property "=key=" c1))
	   (key2 (literef-candidate-property "=key=" c2))
	   (timestamp1 (literef-creation-timestamp key1))
	   (timestamp2 (literef-creation-timestamp key2)))
      (unless timestamp1 (throw 'ok 1))
      (unless timestamp2 (throw 'ok -1))
      (cond
       ((> timestamp1 (+ timestamp2 literef-equal-timestamps)) 1)
       ((> timestamp2 (+ timestamp1 literef-equal-timestamps)) -1)
       (t 0)))))

(defun literef-str-compare(c1 c2 property)
  "Compare the PROPERTY of the candidates C1 and C2. If the property is specified for both candidates, it is assumed to be of string type."
  (catch 'ok
    (let* ((s1 (literef-candidate-property property c1))
	   (s2 (literef-candidate-property property c2)))
      (unless s1 (throw 'ok 1))
      (unless s2 (throw 'ok -1))
      (literef-raw-str-compare s1 s2))))

(defun literef-timestamp-up(c1 c2)
  "Compare timestamps of the candidates C1 and C2 using `literef-timestamp-compare'."
  (literef-timestamp-compare c1 c2))

(defun literef-timestamp-down(c1 c2)
  "Compare timestamps of the candidates C1 and C2 using the inverse of `literef-timestamp-compare'."
  (- (literef-timestamp-up c1 c2)))

(defun literef-key-up(c1 c2)
  "Compare the key property of the candidates C1 and C2 using `literef-str-compare'."
  (literef-str-compare c1 c2 "=key="))
(defun literef-key-down(c1 c2)
  "Compare the key property of the candidates C1 and C2 using the inverse of `literef-str-compare'."
  (- (literef-key-up c1 c2)))

(defun literef-author-up(c1 c2)
  "Compare the authors property of the candidates C1 and C2 using `literef-str-compare'."
  (literef-str-compare c1 c2 "author"))
(defun literef-author-down(c1 c2)
  "Compare the authors property of the candidates C1 and C2 using the inverse of `literef-str-compare'."
  (- (literef-author-up c1 c2)))

(defun literef-title-up(c1 c2)
  "Compare the title property of the candidates C1 and C2 using `literef-str-compare'."
  (literef-str-compare c1 c2 "title"))
(defun literef-title-down(c1 c2)
  "Compare the title property of the candidates C1 and C2 using the inverse of `literef-str-compare'."
  (- (literef-title-up c1 c2)))

(defun literef-venue-up(c1 c2)
  "Compare the venue property of the candidates C1 and C2 using `literef-raw-str-compare'."
  (literef-raw-str-compare
   (literef-candidate-venue c1)
   (literef-candidate-venue c2)))
(defun literef-venue-down(c1 c2)
  "Compare the venue property of the candidates C1 and C2 using the inverse of `literef-raw-str-compare'."
  (- (literef-venue-up c1 c2)))

(defun literef-type-up(c1 c2)
  "Compare the venue type property of the candidates C1 and C2 using `literef-raw-str-compare'."
   (literef-raw-str-compare
    (literef-candidate-venue-type c1)
    (literef-candidate-venue-type c2)))
   
(defun literef-type-down(c1 c2)
  "Compare the venue property of the candidates C1 and C2 using the inverse of `literef-raw-str-compare'."
  (- (literef-type-up c1 c2)))

(defun literef-year-up(c1 c2)
  "Compare the year property of the candidates C1 and C2 using `literef-str-compare'."
  (literef-str-compare c1 c2 "year"))
(defun literef-year-down(c1 c2)
  "Compare the year property of the candidates C1 and C2 using the inverse of `literef-str-compare'."
  (- (literef-year-up c1 c2)))

(defun literef-compare(c1 c2)
  "Compare the candidates C1 and C2 based on the value of the variable `literef-criteria'."
  (let (res)
    (catch 'ok
      (dolist (c literef-criteria res)
	(let ((myres (funcall c c1 c2)))
	  (when (not (= myres 0))
	    (throw 'ok (< myres 0))))))))

(defun literef-char-to-compare(char)
  "Return the comparison function symbol corresponding to CHAR, which is assumed to be one of the characters as in the variable `literef-citation-link-sorting-criteria'. If CHAR is not one of those characters, then return nil."
  (let ((compare-map
	 '(?k literef-key-up ?K literef-key-down
	      ?d literef-timestamp-up ?D literef-timestamp-down
	      ?a literef-author-up ?A literef-author-down
	      ?t literef-title-up ?T literef-title-down
	      ?v literef-venue-up ?V literef-venue-down
	      ?w literef-type-up ?W literef-type-down
	      ?y literef-year-up ?Y literef-year-down)))
    (when (stringp char) (setq char (string-to-char char)))
    (plist-get compare-map char)))

(defun literef-criteria-list(chars)
  "Compute the list of comparison functions from the list of characters CHARS as in the variable `literef-citation-link-sorting-criteria'."
  (let (res)
    (dolist (c chars nil)
      (let ((compare (literef-char-to-compare c)))
	(when compare (push compare res))))
    (reverse res)))

(defun literef-read-sorting-criteria()
  "Handle interactive input of the search criteria."
  (unwind-protect
      (progn
	(org-ref-cancel-link-messages)
	(let (res
	      (str-res "")
	      (str-map '(?k "Key↑" ?K "Key↓"
			    ?d "Creation-Date↑" ?D "Creation-Date↓"
			    ?a "Author↑" ?A "Author↓"
			    ?t "Title↑" ?T "Title↓"
			    ?v "Venue↑" ?V "Venue↓"
			    ?w "Venue-Type↑" ?W "Venue-Type↓"
			    ?y "Year↑" ?Y "Year↓"
			    )))
	  (catch 'exit
	    (while t
	      (let* ((ans (read-char (concat
				      "Current criteria:" str-res "\n"
				      (format
				       (concat "%-25s    %s\n"
					       "%-25s    %s\n"
					       "%-25s    %s\n"
					       "%-25s    %s\n")
				       "Key↑ (k) | Key↓ (K)"
				       "Creation Date↑ (d) | Creation Date↓ (D)"
				       "Author↑ (a) | Author↓ (A)"
				       "Title↑ (t) | Title↓ (T)"
				       "Venue↑ (v) | Venue↓ (V)"
				       "Venue Type↑ (w) | Venue Type↓ (W)"
				       "Year↑ (y) | Year↓ (Y)" ""))))
		     (str (plist-get str-map ans)))
		(cond
		 ((eq ans ?\r) (throw 'exit nil))
		 ((eq ans ?\e) (setq res nil) (throw 'exit nil))
		 (t
		  (when (literef-char-to-compare ans)
		    (setq str-res (concat str-res " " str))
		    (push ans res)))))))
	  (literef-criteria-list (reverse res))))
    (progn
      (org-ref-show-link-messages)
      (setq org-ref-show-citation-on-enter t))))

(defvar literef-criteria nil
  "The criteria for sorting candidates. See the variable `literef-citation-link-sorting-criteria' for the possible values.")

(defun literef-sort (_orig-fun)
  "Sort the candidates."
  (interactive)
  (setq literef-criteria (literef-read-sorting-criteria))
  (helm-update))

(defun literef-filtered-candidate-transformer
    (_orig-fun candidates &optional _source)
  "Transform the candidates list CANDIDATES by sorting."
  (if (and (boundp 'literef-criteria) literef-criteria)
      (-sort 'literef-compare candidates)
  candidates))

(advice-add 'org-ref-helm-candidate-transformer
	    :around #'literef-filtered-candidate-transformer)

(advice-add 'org-ref-helm-cite-sort
	    :around #'literef-sort)

;;;; Candidate actions

(defun literef-helm-marked-keys()
  "Computes the list of keys that are currently marked in `helm'."
  (let (keys)
    (dolist (entry (helm-marked-candidates) nil)
      (push (cdr (assoc "=key=" entry)) keys))
    (reverse keys)))

(defun literef-helm-insert-action(_c)
  "The insert-action of `helm'."
  (if (boundp 'literef-helm-no-insert)
      (car (literef-helm-marked-keys))
    (dolist (key (literef-helm-marked-keys) nil)
      (insert-for-yank key))))

(defun literef-kill-ring-action-yank(orig_fun _string)
  "The LiteRef version of `helm-kill-ring-action-yank', which performs intelligent yanking of keys."
  (dolist (c (helm-marked-candidates) nil)
    (insert-for-yank c)))

(advice-add 'helm-kill-ring-action-yank
	    :around 'literef-kill-ring-action-yank)

(defun literef-action-transformer (_orig-fun actions candidate)
  "Transform the initial list of ACTIONS associated with the CANDIDATE."
  (let ((key (literef-candidate-property "=key=" candidate)))
    (push 
     (cons "Insert citation." 'literef-helm-insert-action) actions)
    (push 
     (cons "Open notes." `(lambda(_c) (literef-open-key-notes ,key)))
     actions)
    (push 
     (cons "Open PDF." `(lambda(_c) (literef-open-key-pdf ,key)))
     actions)
    (push 
     (cons "Build citation sub-graph." `(lambda(_c) (literef-select-subgraph ,key)))
     actions)
    )
  (reverse actions))

(advice-add 'org-ref-helm-cite-action-transformer
	    :around #'literef-action-transformer)


;;;; Fallback options

;; For now, no fallback options.
;; Future work -- implement it using the ideas from bibtex-completion.el
(setq org-ref-helm-cite-fallback-source nil)

;;;; Citing from the selected subgraph
(defun literef-subgraph-helm()
  "The version of `org-ref-helm-cite', in which the candidates are restricted to the selected subgraph."
  (interactive)
  (let* ((current-subgraph literef-subgraph)
	 (literef-subgraph-helm t))
    (org-ref-helm-cite)))

(provide 'literef-helm)
