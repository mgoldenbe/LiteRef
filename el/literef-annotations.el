(require 'company)
(require 'cl-lib)

(defun literef-citation-function-help-echo(window object position)
  (save-excursion
    (goto-char position)
    (goto-char (org-element-property :end (org-element-context)))
    (format "%s%s%s" "The current paper relates to the cited one as: " (match-string 2) ".")))
    
(org-link-set-parameters
 literef-citation-function-link
 :follow (lambda (path) (message "%s%s%s" "The current paper relates to the cited one as: " path "."))
 :export (lambda (path desc backend) "") ; ignore the link
 :face `(:foreground ,literef-citation-function-color)
 :help-echo 'literef-citation-function-help-echo)

(defun literef-input-functions()
  "Input citation functions."
  (completing-read-multiple "Select or more comma-separated citation functions (press TAB for completion):\n" literef-citation-functions))

(defun literef-citation-function()
  "Annotate a citation with user-selected citation functions."
  (interactive)
  (let ((link (org-element-context)))
    (if (literef-citation-link-p link)
	(progn
	  (goto-char (literef-link-end link))
	  (insert " " literef-citation-function-link ":"
		  (literef-join-strings (literef-input-functions) ",")))
      (message "There is no citation link to annotate."))))

;; (defun literef-citation-functions(key)
;;   "Compute a mapping, where keys are citations in the notes associated with KEY and values are functions associated with each citation."
;;   (let ((res (make-hash-table :test 'equal)))
;;     (with-temp-buffer
;;       (insert-file-contents (literef-notes-filename key))
;;       (dolist (annotation-link (literef-citation-function-links) nil)
;; 	(let ((citation-link
;; 	       (literef-link-prev-element annotation-link)))
;; 	  (when (literef-citation-link-p citation-link)

;; 	    (dolist (key (literef-link-path-components citation-link) nil)
;; 	      (let ((functions-hash
;; 		     (gethash key res (make-hash-table :test 'equal))))
;; 		(dolist (function
;; 			 (literef-link-path-components annotation-link)
;; 			 nil)
;; 		  (puthash function t functions-hash))
;; 		(puthash key functions-hash res)))))))
;;     res))

;; (defun literef-dump-citation-functions()
;;   "Just for debugging of 'literef-citation-functions. Create a list of citations with their functions for the current key."
;;   (interactive)
;;   (let ((key (literef-current-key)))
;;     (if key
;; 	(let ((res-hash (literef-citation-functions key))
;; 	      (res))
;; 	  (dolist (pair
;; 		   (literef-hash-pairs-to-list
;; 		    (literef-citation-functions
;; 		     "Bouzy2013c-Monte-Carlo"))
;; 		   res)
;; 	    (push (elt pair 0) res)
;; 	    (dolist (function
;; 		     (literef-hash-keys-to-list (elt pair 1))
;; 		     nil)
;; 	      (push function res)))
;; 	  (setq res (reverse res))
;; 	  (message "%s" res))
;;        nil)))

;; (defun literef-list-satisfies-predicate-p(predicate list)
;;   "Check whether the LIST satisfies the PREDICATE."
;;   (dolist (f list nil)
;;     (setq predicate
;; 	  (literef-replace-in-string-whole-words f  "t" predicate)))
;;   (dolist (f literef-citation-functions nil)
;;     (setq predicate
;; 	  (literef-replace-in-string-whole-words f "nil" predicate)))
;;   (condition-case nil
;;       (literef-eval-string predicate)
;;     (error (error "Could not evaluate: %s" predicate))))

;; ;; This function is based on http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend
;; (defun literef-company-annotations-backend
;;     (command &optional arg &rest ignored)
;;   (interactive (list 'interactive))
;;   (let ((my-completions (copy-sequence literef-citation-functions)))
;;     (cl-case command
;;       (interactive (company-begin-backend 'literef-company-annotations-backend))
;;       (prefix (and t ;(eq major-mode 'fundamental-mode)
;; 		   (company-grab-symbol)))
;;       (candidates
;;        (cl-remove-if-not
;; 	(lambda (c) (string-prefix-p arg c))
;; 	my-completions)))))

;; (defun literef-minibuffer-mode()
;;   (company-mode 1))

;; ;; (define-key company-mode-map (kbd "TAB") 'literef-company-annotations-backend)

;; (defun literef-read-query(prompt)
;;   "Read query with completion from 'literef-citation-functions."
;;   (let ((company-backends (copy-sequence company-backends))
;; 	(minibuffer-local-map (copy-sequence minibuffer-local-map))
;; 	(minibuffer-setup-hook (copy-sequence minibuffer-setup-hook))
;; 	(resize-mini-windows t))
;;     (add-to-list 'company-backends 'literef-company-annotations-backend)
;;     (define-key minibuffer-local-map (kbd "TAB") 'literef-company-annotations-backend)
;;     (add-hook 'minibuffer-setup-hook 'literef-minibuffer-mode)
;;     (read-from-minibuffer prompt)))

;; (defun literef-outgoing-citations()
;;   "Search the notes of the current key for citations with citation functions matching the query."
;;   (interactive)
;;   (let ((query (literef-read-query
;; 		"Search for outgoing citations by citation function:\n"))
;; 	(key (literef-current-key)))
;;     (if key
;; 	(let ((res-hash (literef-citation-functions key))
;; 	      (res))
;; 	  (dolist (pair
;; 		   (literef-hash-pairs-to-list res-hash)
;; 		   nil)
;; 	    (when (literef-list-satisfies-predicate-p
;; 		   query
;; 		   (literef-hash-keys-to-list (elt pair 1)))
;; 	      (push (elt pair 0) res)))
;; 	  (setq res (literef-join-strings (sort res 'string<) ","))
;; 	  (message "The following citation link is in the kill ring:\n%s"
;; 		   res)
;; 	  (kill-new res))
;;       nil)))

;; (defun literef-incoming-citations()
;;   "Search the notes for citations of the current key with citation functions matching the query."
;;   (interactive)
;;   (let ((query (literef-read-query
;; 		"Search for incoming citations by citation function:\n"))
;; 	(key (literef-current-key)))
;;     (if key
;; 	(let (res)
;; 	  (dolist (source-key (literef-all-keys) nil)
;; 	    (let* ((res-hash (literef-citation-functions source-key))
;; 		   (functions-hash (gethash key res-hash))
;; 		   (functions (if functions-hash
;; 				  (literef-hash-keys-to-list functions-hash)
;; 				nil)))
;; 	      (when (and
;; 		     functions (literef-list-satisfies-predicate-p
;; 				query
;; 				functions))
;; 		(push source-key res))))
;; 	  (setq res (literef-join-strings (sort res 'string<) ","))
;; 	  (message "The following citation link is in the kill ring:\n%s"
;; 		   res)
;; 	  (sleep-for 2)
;; 	  (kill-new res))
;;       nil)))

(provide 'literef-annotations)


