(require 'smooth-scrolling)

;;;; Collecting citation links and functions from a buffer or from notes of a key.
(defun literef-add-citation-and-functions-to-hash
    (citation-link functions hash)
  "Add keys in CITATION-LINK and the citation FUNCTIONS to HASH and return the latter."
  (dolist (key (literef-link-path-components citation-link) nil)
    (let ((functions-hash
	   (gethash key hash (make-hash-table :test 'equal))))
      (dolist (f functions nil)
	(puthash f t functions-hash))
      (puthash key functions-hash hash)))
  hash)

(defun literef-out-citations()
  "For the current buffer, compute a mapping, where keys are citations and values are functions associated with each citation."
  (org-export-expand-include-keyword)
  (let ((res (make-hash-table :test 'equal)))
    ;; Handle citations that have annotation links.
    (dolist (annotation-link (literef-citation-function-links) nil)
      (let ((citation-link
	     (literef-backward-adjacent-org-element annotation-link)))
	(when (literef-citation-link-p citation-link)	  
	    (let ((functions
		   (literef-link-path-components annotation-link)))
	      (setq res
		    (literef-add-citation-and-functions-to-hash
		     citation-link functions res))))))
    ;; Handle citations that do not have annotation links.
    (dolist (citation-link (literef-citation-links) nil)
      (setq res
	    (literef-add-citation-and-functions-to-hash
	     citation-link nil res)))
    ;; Exchange hash tables of citation functions for lists.
    (dolist (pair (literef-hash-pairs-to-list res) res)
      (puthash
       (elt pair 0)
       (sort (literef-hash-keys-to-list (elt pair 1)) 'string<)
       res))))

(defun literef-key-out-citations(key)
  "Compute a mapping, where keys are citations in the notes associated with KEY and values are functions associated with each citation."
  (with-temp-buffer
    (insert-file-contents (literef-notes-filename key))
    (literef-out-citations)))

(defun literef-dump-out-citations()
  "Just for debugging of 'literef-out-citations. Create a list of citations with their functions for the current buffer."
  (interactive)
  (let ((res-hash (literef-out-citations))
	(res))
    (dolist (pair
	     (literef-hash-pairs-to-list res-hash)
	     res)
      (push (cons (elt pair 0) (elt pair 1)) res))
    (setq res (reverse res))
    (message "%s" res))
  nil)

;;;; Building the graph of keys with citations as arcs.
;;;; The building is done in a way that supports incremental updates.
;;;; Each arc is labeled with citation functions.
;;;; The vertices are stored in a hash table.
;;;; In and out-adjacency lists are hash tables.
;;;; In the out-adjacency, each key is mapped to a list of citation functions.
;;;; In the in-adjacency, each key is mapped to t. So, we do not store the citation functions twice and do not require syncronization of the two ends of an arc. However, this requires an extra look-up to find the list of citation functions for an in-neighbor.

(defun literef-graph-add-key(key &optional graph)
  "Add KEY to GRAPH. If GRAPH is not specified, it is `literef-graph'."
  (unless graph (setq graph literef-graph))  
  (puthash
   key
   (cons
    (make-hash-table :test 'equal) ; out-adjacency
    (make-hash-table :test 'equal)) ; in-adjacency
   graph))

(defun literef-graph-add-arc(from-key to-key citation-functions
					       &optional graph)
  "Add an arc in GRAPH from FROM-KEY to TO-KEY labeled by CITATION-FUNCTIONS. If GRAPH is not specified, it is `literef-graph'."
  (unless graph (setq graph literef-graph))
  (let ((out-neighbors (literef-key-out-neighbors from-key graph))
	(in-neighbors (literef-key-in-neighbors to-key graph)))
    (puthash to-key citation-functions out-neighbors)
    (puthash from-key t in-neighbors)))

(defun literef-init-graph(&optional init-keys)
  "Initialize the graph of keys using INIT-KEYS as the list of keys. If INIT-KEYS is not specified, all of the keys are used."
  (unless init-keys (setq init-keys (literef-all-keys)))
  (let ((res (make-hash-table :test 'equal)))
    (dolist (key init-keys res)
      (literef-graph-add-key key res))))

(defun literef-key-out-neighbors(key &optional graph)
  "The out-neighbors of KEY in the GRAPH of keys. If GRAPH is not specified, it is `literef-graph'."
  (unless graph (setq graph literef-graph))
  (car (gethash key graph)))

(defun literef-key-in-neighbors(key &optional graph)
  "The in-neighbors of KEY in the GRAPH of keys. If GRAPH is not specified, it is `literef-graph'."
  (unless graph (setq graph literef-graph))
  (cdr (gethash key graph)))

(defun literef-graph-arc-label(from-key to-key &optional graph)
  "Return the citation functions by which an arc in GRAPH from FROM-KEY to TO-KEY is labeled. If GRAPH is not specified, it is `literef-graph'."
  (unless graph (setq graph literef-graph))
  (gethash to-key (literef-key-out-neighbors from-key graph)))

(defun literef-graph-update-key(key)
  "Update the graph of keys with the changes in the notes of KEY."
  (let ((out-neighbors (literef-key-out-citations key)))
    ;; For citations that got removed, remove the corresponding in-arcs.
    (dolist (out-neighbor
	     (literef-hash-keys-minus
	      (literef-key-out-neighbors key)
	      out-neighbors) nil)
      (let ((out-neighbor-in-neighbors
	     (literef-key-in-neighbors out-neighbor)))
	(remhash key out-neighbor-in-neighbors)))
    ;; Make sure key is present in in-adjacency of all out-neighbors
    (dolist (out-neighbor (literef-hash-keys-to-list out-neighbors)
			  nil)
      (puthash key t (literef-key-in-neighbors out-neighbor)))
    ;; Substitute out-neighbors
    (puthash key
	     (cons out-neighbors (literef-key-in-neighbors key))
	     literef-graph)))

(defun literef-save-hook()
  "Update the graph of keys when notes of a key are saved"
  (let ((key (literef-current-buffer-key)))
    (when key
      (literef-graph-update-key key)
      (message "The graph of keys is updated. You may want to update the subgraph selection."))))

(add-hook 'after-save-hook 'literef-save-hook)

(defun literef-compute-graph()
  "Compute the graph of keys from scratch."
  (interactive)
  (setq literef-graph (literef-init-graph))
  (dolist (key (literef-all-keys) literef-graph)
    (literef-graph-update-key key)))

(defvar literef-graph (literef-compute-graph)
  "The graph of keys.")

;;;; Arc filter for subgraph selection.

(defun literef-list-satisfies-predicate-p(predicate list)
  "Check whether the LIST satisfies the PREDICATE."
  (dolist (f list nil)
    (setq predicate
	  (literef-replace-in-string-whole-words f  "t" predicate)))
  (dolist (f literef-citation-functions nil)
    (setq predicate
	  (literef-replace-in-string-whole-words f "nil" predicate)))
  (condition-case nil
      (literef-eval-string predicate)
    (error (error "Could not evaluate: %s" predicate))))

(defun literef-arc-filter-variables()
  "Return the list of variables recognized by the arc filter."
  (append
   (mapcar (lambda(f)
	     (concat literef-arc-filter-variables-prefix f))
	   literef-citation-functions)
   (mapcar (lambda(f)
	     (concat literef-arc-filter-variables-prefix f))
	   (list "in" "out" "depth"))))

;; This function is based on http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend
(defun literef-arc-filter-company-backend
    (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (let ((my-completions (literef-arc-filter-variables)))
    (cl-case command
      (interactive (company-begin-backend 'literef-arc-filter-company-backend))
      (prefix (and t ;(eq major-mode 'fundamental-mode)
		   (company-grab-symbol)))
      (candidates
       (cl-remove-if-not
	(lambda (c) (string-prefix-p arg c))
	my-completions)))))

(defun literef-arc-filter-minibuffer-mode()
  (company-mode 1))

(defun literef-read-arc-filter(prompt)
  "Read query with completion from 'literef-citation-functions."
  (let ((company-backends (copy-sequence company-backends))
	(minibuffer-local-map (copy-sequence minibuffer-local-map))
	(minibuffer-setup-hook (copy-sequence minibuffer-setup-hook))
	(resize-mini-windows t))
    (add-to-list 'company-backends 'literef-arc-filter-company-backend)
    (define-key minibuffer-local-map (kbd "TAB") 'literef-arc-filter-company-backend)
    (add-hook 'minibuffer-setup-hook 'literef-arc-filter-minibuffer-mode)
    (read-from-minibuffer prompt)))

(defun literef-make-arc-filter(str)
  "Make and evaluate defun for the function named literef-arc-filter-p for the filter in the string STR."
  (with-temp-buffer
    (insert "(defun literef-arc-filter-p()\n")
    (insert "    (let (")
    (dolist (var-name (literef-arc-filter-variables) nil)
      (insert "\n          (" var-name " literef-temp-" var-name ")"))
    (insert ")\n       " str "))")
    ;; (message "Here:\n%s" (buffer-string)) 
    (eval-buffer)))

;;;; Selection of the subgraph.

(defvar literef-subgraph literef-graph
  "The selected subgraph")

(defun literef-arc-filter-p()
  "Returns t if the current arc satisfies the filter and nil otherwise.

This function is re-defined by `literef-make-arc-filter'.")

;; The default arc filter.
(literef-make-arc-filter "t")

(defun literef-select-initial-keys()
  "Select initial keys for forming the subgraph."
  (let* ((current-key (literef-current-key))
	 (ans (literef-read-char
	       (concat "Choose the source of initial keys:  "
		       "All keys (a)  |  Current buffer (b)"
		       (when current-key
			 (concat "  |  " current-key "(c)")))
	       (if current-key '(?a ?b ?c) '(?a ?b)))))
    (cond ((eq ans ?a)
	   (setq literef-subgraph-source :all-keys)
	   (literef-all-keys))
	  ((eq ans ?b)
	   (setq literef-subgraph-source :buffer)
	   (literef-hash-keys-to-list (literef-out-citations)))
	  ((eq ans ?c)
	   (setq literef-subgraph-source :current-key)
	   (list current-key)))))

(defun literef-arc-filter-temp-variable(str)
  "Return the symbol named literef-temp-<prefix><str>."
  (intern (concat "literef-temp-"
		  literef-arc-filter-variables-prefix str)))

(defun literef-graph-check-arc(from-key
			       to-key-cons &optional init-flag)
  "Return true if the arc represented by FROM-KEY and TO-KEY-CONS (consisting of a key and a plist) fits the arc filter."
  (let ((to-key (car to-key-cons))
	(to-properties (cdr to-key-cons)))
    ;; Set the special variables for the arc filter to work.
    (set (literef-arc-filter-temp-variable "depth")
	 (plist-get to-properties :depth))
    (set (literef-arc-filter-temp-variable "out")
	 (eq (plist-get to-properties :direction) :out))
    (set (literef-arc-filter-temp-variable "in")
	 (eq (plist-get to-properties :direction) :in))
    (dolist (f literef-citation-functions nil)
      (set (literef-arc-filter-temp-variable f) nil))
    (dolist (f (plist-get to-properties :functions) nil)
      (set (literef-arc-filter-temp-variable f) t))
    (literef-arc-filter-p)))

(defun literef-add-to-next-iter(from-key to-key-cons)
  "Handle the arc represented by FROM-KEY and TO-KEY-CONS (consisting of a key and a plist). If the arc fits the filter, the to-key (the car component of TO-KEY-CONS) is inserted in the subgraph. If, in addition, the duplicate detection is passed, then add to-key to the keys for the next iteration."
  (let ((to-key (car to-key-cons))
	(to-properties (cdr to-key-cons)))
    (when (literef-graph-check-arc from-key to-key-cons)
      (unless (gethash to-key literef-subgraph nil)
	(puthash to-key to-properties next-iter)
	(literef-graph-add-key to-key literef-subgraph))
      (let ((functions (plist-get to-properties :functions))
	    (direction (plist-get to-properties :direction)))
	(if (eq direction :out)
	    (literef-graph-add-arc
	     from-key to-key functions literef-subgraph)
	  (literef-graph-add-arc
	   to-key from-key functions literef-subgraph))))))

(defun literef-neighbor-pairs(cur-direction next-direction cur-properties)
  "Compute the neighbors pairs consisting of key and properies that fit the direction of the key being expanded (CUR-DIRECTION) the required direction of the arc (NEXT-DIRECTION) and the properties of the key being expanded (CUR-PROPERTIES)."
  (let (res)
    (when (or (not cur-direction) (eq cur-direction next-direction))
      (dolist (pair
	       (literef-hash-pairs-to-list
		(if (eq next-direction :out)
		    (literef-key-out-neighbors key)
		  (literef-key-in-neighbors key)))
	       res)
	(let ((properties (copy-seq cur-properties)))
	  (setq properties
		(plist-put properties :depth
			   (1+ (plist-get cur-properties :depth))))
	  (setq properties
		(plist-put properties :direction next-direction))
	  (setq properties
		(plist-put properties :functions
			   (if (eq next-direction :out)
			       (literef-graph-arc-label
				key (elt pair 0))
			     (literef-graph-arc-label
			      (elt pair 0) key))))
	  (push (cons (car pair) properties) res))))))

(defun literef-uniform-cost-search(initial-keys)
  "Performs uniform-cost search from INITIAL-KEYS while respecting the current arc filter. The resulting subgraph is accumulated in literef-subgraph."
  (let ((cur-iter (make-hash-table :test 'equal))
	(next-iter (make-hash-table :test 'equal)))
    (dolist (key initial-keys nil)
      (puthash key
	       (list :direction nil :depth 0 :expanded nil) next-iter))
    (while (not (literef-hash-empty-p next-iter))
      (setq cur-iter next-iter)
      (setq next-iter (make-hash-table :test 'equal))
      (dolist (pair (literef-hash-pairs-to-list cur-iter) nil)
	(let* ((key (elt pair 0))
	       (properties (elt pair 1))
	       (depth (plist-get properties :depth))
	       (direction (plist-get properties :direction)))
	  (remhash key cur-iter)
	  (setq properties (plist-put properties :expanded t))
	  (dolist (pair
		   (nconc
		    (literef-neighbor-pairs direction :out properties)
		    (literef-neighbor-pairs direction :in properties))
		   nil)
	    (literef-add-to-next-iter key pair)))))
    nil))

(defun literef-select-subgraph()
  "Select subgraph of the graph of keys. Sets `literef-subgraph-keys'."
  (interactive)
  (unwind-protect
      (progn
	(org-ref-cancel-link-messages)
	(let ((initial-keys (literef-select-initial-keys)))
	  (literef-make-arc-filter (literef-read-arc-filter
				    "Enter a predicate for the arc filter: \n"))
	  (setq literef-subgraph (literef-init-graph initial-keys))
	  (when (eq literef-subgraph-source :buffer)
	    (let ((buffer-node-name
		   (concat "Buffer \"" (buffer-name) "\"")))
	      (literef-graph-add-key buffer-node-name literef-subgraph)
	    (dolist (key initial-keys nil)
	      (literef-graph-add-arc
	       buffer-node-name key nil literef-subgraph))))
	  (literef-uniform-cost-search initial-keys)))
    (progn
      (org-ref-show-link-messages)
      (setq org-ref-show-citation-on-enter t)))
  (literef-show-graph)
  literef-subgraph)

;;;; Operations on the selected subgraph.

(defvar literef-graph-mode-map nil
  "Key map for literef-graph-mode")

(defun literef-graph-scroll-right()
  "Handle scrolling right."
  (interactive)
  (scroll-left 1))

(defun literef-graph-scroll-left()
  "Handle scrolling left."
  (interactive)
  (scroll-right 1))

(defun literef-graph-scroll-up()
  "Handle scrolling right."
  (interactive)
  (scroll-up 1))

(defun literef-graph-scroll-down()
  "Handle scrolling left."
  (interactive)
  (scroll-down 1))

(progn
  (setq literef-graph-mode-map (make-sparse-keymap))
  (define-key literef-graph-mode-map (kbd "M-<right>")
    'literef-graph-scroll-right)
  (define-key literef-graph-mode-map (kbd "M-<left>")
    'literef-graph-scroll-left)
  (define-key literef-graph-mode-map (kbd "M-<up>")
    'literef-graph-scroll-up)
  (define-key literef-graph-mode-map (kbd "M-<down>")
    'literef-graph-scroll-down)
  )

(define-minor-mode literef-graph-mode
  "LiteRef mode for viewing the selected subgraph."
  nil " LiteRefGraph" literef-graph-mode-map)  

(defun literef-graph-string()
  "Return a string representation of the keys graph."
  (let ((res "graph { flow: south; }\n")) ;; http://bloodgate.com/perl/graph/manual/hinting.html
    (dolist (key (literef-hash-keys-to-list literef-subgraph) nil)
      (let ((out-neighbors
	     (literef-key-out-neighbors key literef-subgraph)))
	(dolist (out-neighbor
		 (literef-hash-pairs-to-list out-neighbors)
		 nil)
	  (setq res (concat
		     res "[ "
		     (when (literef-key-exists key) "cite:")
		     key " ]"
		     (let ((functions
			    (mapcar
			     (lambda(f) (concat
					 literef-citation-function-link
					 ":" f))
			     (elt out-neighbor 1))))
		       (if functions
			   (concat
			    " -- "
			    (literef-join-strings functions "\\n"))
			 ""))
		     " --> " "[ cite:" (elt out-neighbor 0)  " ]\n")))))
    res))

(defun literef-show-graph()
  "Show the graph of keys."
  (interactive)
  (save-selected-window
    (let ((temp-file-name (make-temp-file "foo")))
      (with-temp-file temp-file-name
	(insert (literef-graph-string)))
      (let* ((command (concat "graph-easy --boxart" " " temp-file-name))
	     (graph-layout (shell-command-to-string command)))
	;; (delete-file temp-file-name)
	(switch-to-buffer-other-window "The graph of keys.")
	(org-mode)
	(literef-graph-mode 1)
	(setq-local auto-hscroll-mode nil)
	(smooth-scrolling-mode 1)
	
	;; Do not wrap lines
	(setq-local truncate-lines t)

	;;;; The following settings are for ASCII output. For BoxArt, they are not needed, but are still fine.
	;; Do not highlight as in tabular mode.
	;; https://emacs.stackexchange.com/a/13955/16048
	(setq-local org-enable-table-editor nil)
	(setq-local face-remapping-alist
		    (cons '(org-table . default) face-remapping-alist))
	;; Do not use strike-through.
	;; https://stackoverflow.com/a/22493885/2725810
	(setq-local org-emphasis-alist nil)

	;; Make sure nothing remains from the last evaluation.
	(setq-local buffer-read-only nil)
	(erase-buffer)

	;; Make sure that the font is monospace.
	(face-remap-add-relative 'default
				 :family "Monospace"
				 :height literef-graph-font-height)
	
	;; At last insert the layout.
	(insert (substring graph-layout 0 -1))

	;; Smooth horizontal scrolling.
	(setq-local hscroll-margin 0)
	(setq-local hscroll-step 1)
	;; Add spaces, so horizontal scrolling is available in all lines.
	(literef-append-spaces (literef-longest-line-length))

	;; Make the buffer read-only.
	(setq-local buffer-read-only t)
	)))
    nil)

(defun literef-longest-line-length()
  "Compute the length of the longest line in the buffer."
  (save-excursion
    (let ((res 0))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((cur-len (- (line-end-position) (line-beginning-position))))
	  (setq res (max res cur-len)))
	(forward-line))
      res)))

(defun literef-append-spaces(required-length)
  "Append spaces to all lines, so they become at least the given REQUIRED-LENGTH long."
  (save-excursion
    (let ((res 0))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((cur-len (- (line-end-position) (line-beginning-position))))
	  (goto-char (line-end-position))
	  (insert (make-string (- required-length cur-len) ?\s)))
	(forward-line))
      nil)))

(provide 'literef-graph)
