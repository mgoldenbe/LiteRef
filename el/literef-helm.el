(require 'helm)
(require 'cl)
(require 'dash)

(defun literef-assoc(property a-list)
  "Return either nil or the value in A-LIST associated with the PROPERTY."
  (let ((raw (assoc property a-list)))
    (if raw (cdr raw) nil)))

(defun literef-candidate-property(property c)
  "Return either nil or the PROPERTY of the candidate C."
  (let ((fields (cdr c)))
    (literef-assoc property fields)))

(defun literef-candidate-venue(c)
  "Return either nil or the venue of of the candidate C."
  (or (literef-candidate-property "booktitle" c)
      (literef-candidate-property "journal" c)))

;;;; Changing the candidate strings.

(defun literef-candidate-helm-string(c)
  "Compute the string to be displayed in helm for a candidate C"
  (let* ((res "")
	 (fields (cdr c))
	 (key (literef-assoc "=key=" fields))
	 (creation-date (format-time-string
			 "%d-%m-%Y %H:%M:%S"
			 (seconds-to-time (literef-creation-timestamp key))))
	 (author (literef-assoc "author" fields))
	 (title (literef-assoc "title" fields))
	 (venue (literef-candidate-venue c)))
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

(defun literef-candidate-transformer(candidates &optional)
  "Transform the initial candidate list for helm."
  (let (res)
    (dolist (c candidates res)
      (push (cons (literef-candidate-helm-string c) (cdr c)) res))
    (setq res (reverse res))
    res
    ))

;; Filtering wouldn't work correctly unless
;; candidates are transformed at the beginning.
(push '(candidate-transformer . literef-candidate-transformer)
      org-ref-helm-cite-source)

;;;; Sorting

(defun literef-timestamp-compare(c1 c2)
  "Compare two candidates based on timestamps. Consider timestamps within `literef-equal-timestamps` seconds as equal."
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

(defun literef-raw-str-compare(s1 s2)
  "Compare two strings."
  (cond
   ((string< s1 s2) -1)
   ((string< s2 s1) 1)
   (t 0)))

(defun literef-str-compare(c1 c2 property)
  "Compare string PROPERTY of two candidates."
  (catch 'ok
    (let* ((s1 (literef-candidate-property property c1))
	   (s2 (literef-candidate-property property c2)))
      (unless s1 (throw 'ok 1))
      (unless s2 (throw 'ok -1))
      (literef-raw-str-compare s1 s2))))

(defun literef-timestamp-up(c1 c2)
  "Compare timestamps of candidates for storing in increasing order."
  (literef-timestamp-compare c1 c2))
(defun literef-timestamp-down(c1 c2)
  "Compare timestamps of candidates for storing in decreasing order."
  (- (literef-timestamp-up c1 c2)))

(defun literef-key-up(c1 c2)
  "Compare keys of candidates for storing in increasing order."
  (literef-str-compare c1 c2 "=key="))
(defun literef-key-down(c1 c2)
  "Compare keys of candidates for storing in decreasing order."
  (- (literef-key-up c1 c2)))

(defun literef-author-up(c1 c2)
  "Compare authors for storing in increasing order."
  (literef-str-compare c1 c2 "author"))
(defun literef-author-down(c1 c2)
  "Compare authors for storing in decreasing order."
  (- (literef-author-up c1 c2)))

(defun literef-title-up(c1 c2)
  "Compare titles for storing in increasing order."
  (literef-str-compare c1 c2 "title"))
(defun literef-title-down(c1 c2)
  "Compare titles for storing in decreasing order."
  (- (literef-title-up c1 c2)))

(defun literef-venue-up(c1 c2)
  "Compare venues for storing in increasing order."
  (literef-raw-str-compare
   (literef-candidate-venue c1)
   (literef-candidate-venue c2)))
(defun literef-venue-down(c1 c2)
  "Compare venues for storing in decreasing order."
  (- (literef-venue-up c1 c2)))

(defun literef-type-up(c1 c2)
  "Compare venue types for storing in increasing order."
  (literef-str-compare c1 c2 "=type="))
(defun literef-type-down(c1 c2)
  "Compare venue types for storing in decreasing order."
  (- (literef-type-up c1 c2)))

(defun literef-year-up(c1 c2)
  "Compare years for storing in increasing order."
  (literef-str-compare c1 c2 "year"))
(defun literef-year-down(c1 c2)
  "Compare years for storing in decreasing order."
  (- (literef-year-up c1 c2)))

(defun literef-compare(c1 c2)
  (let (res)
    (catch 'ok
      (dolist (c literef-criteria res)
	(let ((myres (funcall c c1 c2)))
	  (when (not (= myres 0))
	    (throw 'ok (< myres 0))))))))

(defvar literef-current-sorting-criteria nil
  "The current sorting criteria")

(defun literef-read-sorting-criteria()
  "Read search criteria"
  (let (res
	(str-res "")
	(action-map '(?k literef-key-up ?K literef-key-down
			 ?d literef-timestamp-up ?D literef-timestamp-down
			 ?a literef-author-up ?A literef-author-down
			 ?t literef-title-up ?T literef-title-down
			 ?v literef-venue-up ?V literef-venue-down
			 ?w literef-type-up ?W literef-type-down
			 ?y literef-year-up ?Y literef-year-down
			 ))
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
	       (action (plist-get action-map ans))
	       (str (plist-get str-map ans)))
	  (cond
	   ((eq ans ?\r) (throw 'exit nil))
	   ((eq ans ?\e) (setq res nil) (throw 'exit nil))
	   (action
	    (push action res)
	    (setq str-res (concat str-res " " str)))))))
    (if res
	(setq literef-current-sorting-criteria res)
      (setq res literef-current-sorting-criteria))
    (reverse res)))

(defun literef-sort (_orig-fun)
  (interactive)
  (let* ((literef-criteria (literef-read-sorting-criteria)))
    (helm-update)))

(defun literef-filtered-candidate-transformer
    (_orig-fun candidates &optional _source)
  "Transform the candidate list for helm by sorting."
  (if (and (boundp 'literef-criteria) literef-criteria)
      (-sort 'literef-compare candidates)
  candidates))

(advice-add 'org-ref-helm-candidate-transformer
	    :around #'literef-filtered-candidate-transformer)

(advice-add 'org-ref-helm-cite-sort
	    :around #'literef-sort)

;;;; Candidate actions

(defun literef-action-transformer (_orig-fun actions candidate)
  "Transform candidate actions."
  (let ((key (literef-candidate-property "=key=" candidate)))
    (push 
     (cons "Insert citation." `(lambda(_c) (insert-for-yank ,key)))
     actions)
    (push 
     (cons "Open notes." `(lambda(_c) (literef-open-key-notes ,key)))
     actions)
    (push 
     (cons "Open PDF." `(lambda(_c) (literef-open-key-pdf ,key)))
     actions)
    )
  (reverse actions))

(advice-add 'org-ref-helm-cite-action-transformer
	    :around #'literef-action-transformer)
  
(provide 'literef-helm)
