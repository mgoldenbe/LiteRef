;;;; Selection of the subgraph.

;; A subgraph is stored as a list of three properties:
;; :keys -- hash of keys;
;; :generating-arcs -- the arcs traversed to generate the subgraph;
;;                     each key in the hash is of the form:
;;                     <key1><space><key2>.
;; :source -- the source for the search for generating the subgraph.
(defvar literef-subgraph nil
  "The selected subgraph: hash of keys that make it up.")

;; Note: to avoid passing around subgraph,
;; all of the below functions operate on the current subgraph,
;; which should be set before calling them.

(defun literef-subgraph-keys()
  "Return the keys of the current subgraph."
  (plist-get current-subgraph :keys))

(defun literef-subgraph-generating-arcs()
  "Return the generating arcs of the current subgraph."
  (plist-get current-subgraph :generating-arcs))

(defun literef-subgraph-generating-arcs()
  "Return the generating arcs of the current subgraph."
  (plist-get current-subgraph :generating-arcs))

(defun literef-subgraph-source()
  "Return the source of the current subgraph."
  (plist-get current-subgraph :source))

(defun literef-subgraph-source-property(property)
  "Return the PROPERTY of the source of current subgraph."
  (let ((source (literef-subgraph-source)))
    (plist-get source property)))

(defun literef-subgraph-set-source-property(property value)
  "Return the PROPERTY of the source of current subgraph."
  (let ((source (literef-subgraph-source)))
    (literef-plist-put source property value)))

(defun literef-subgraph-add-key(key)
  "Add KEY to the current-subgraph."
  (let ((keys (literef-subgraph-keys)))
    (puthash key t keys)))

(defun literef-key-in-subgraph-p(key)
  "Checks whether KEY is in the current subgraph."
  (let ((keys (literef-subgraph-keys)))
    (when (gethash key keys) t)))

(defun literef-subgraph-add-generating-arc(from-key to-key)
  "Add an arc from FROM-KEY to TO-KEY that was traversed for generating the current subgraph."
  (let ((generating-arcs (literef-subgraph-generating-arcs)))
    (puthash (concat from-key " " to-key) t generating-arcs)))

(defun literef-generating-arc-p(from-key to-key)
  "Checks whether the arc from FROM-KEY to TO-KEY is a generating arc of the current subgraph."
  (let ((generating-arcs (literef-subgraph-generating-arcs)))
    (when (gethash (concat from-key " " to-key) generating-arcs) t)))

(defun literef-init-subgraph()
  "Return a subgraph using INIT-KEYS as the list of keys and return the new subgraph."
  (list :keys (make-hash-table :test 'equal)
	:generating-arcs (make-hash-table :test 'equal)
	:source (list
		 :source-type :all-keys
		 :source-name nil
		 :buffer-node-name nil
		 :filter-string "t")))

(defun literef-subgraph-select-source(&optional key)
  "Select the source for forming the current subgraph. If KEY is specified, it is considered to be the currently active key."
  (let* ((source (literef-subgraph-source))
	 (current-key
	  (if key
	      key
	    (literef-current-key))))
    (if current-key
	(progn
	   (literef-plist-put source :source-type :current-key)
	   (literef-plist-put source :source-name current-key))
      (progn
	(literef-plist-put source :source-type :buffer)
	(literef-plist-put source :source-name (buffer-name))))))

(defun literef-subgraph-initial-keys()
  "Compute initial keys for forming the current subgraph."
  (let ((source-type (literef-subgraph-source-property :source-type))
	(source-name (literef-subgraph-source-property :source-name)))
  (cond ((eq source-type :all-keys)
	 (literef-all-keys))
	((eq source-type :buffer)
	 (condition-case nil
	     (with-current-buffer source-name
	       (literef-hash-keys-to-list (literef-out-citations)))
	   (error (error (concat "The buffer " source-name " does not exist anymore. Consider running literef-subgraph-reset-selection to reset the subgraph selection.")))))
	((eq source-type :current-key)
	   (list source-name)))))

(defun literef-arc-filter-temp-variable(str)
  "Return the symbol named literef-temp-<prefix><str>."
  (intern (concat "literef-temp-"
		  literef-arc-filter-variables-prefix str)))

(defun literef-graph-check-arc(from-key to-key-cons)
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
  "Handle the arc represented by FROM-KEY and TO-KEY-CONS (consisting of a key and a plist). If the arc fits the filter, the to-key (the car component of TO-KEY-CONS) is inserted in the current subgraph. If, in addition, the duplicate detection is passed, then add to-key to the keys for the next iteration."
  (let ((keys (literef-subgraph-keys))
	(to-key (car to-key-cons))
	(to-properties (cdr to-key-cons)))
    (when (literef-graph-check-arc from-key to-key-cons)
      (unless (literef-key-in-subgraph-p to-key)
	(puthash to-key to-properties next-iter)
	(literef-subgraph-add-key to-key))
      (let ((direction (plist-get to-properties :direction)))
	(if (eq direction :out)
	    (literef-subgraph-add-generating-arc from-key to-key)
	  (literef-subgraph-add-generating-arc to-key from-key))))))

(defun literef-neighbor-pairs(cur-direction next-direction cur-properties)
  "Compute the neighbors pairs consisting of key and properies that fit the direction of the key being expanded (CUR-DIRECTION) the required direction of the arc (NEXT-DIRECTION) and the properties of the key being expanded (CUR-PROPERTIES)."
  (let (res)
    (when (or (not cur-direction) (eq cur-direction next-direction))
      (let ((neighbors
	     (literef-hash-pairs-to-list
	      (if (eq next-direction :out)
		  (literef-key-out-neighbors key)
		(literef-key-in-neighbors key)))))
	(dolist (pair neighbors res)
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
	    (push (cons (car pair) properties) res)))))))

(defun literef-uniform-cost-search(initial-keys)
  "Builds the current subgraph by performing uniform-cost search from INITIAL-KEYS while respecting the current arc filter."
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

(defun literef-arc-filter-variables()
  "Return the list of variables recognized by the arc filter."
  (append
   (mapcar (lambda(f)
	     (concat literef-arc-filter-variables-prefix f))
	   literef-citation-functions)
   (mapcar (lambda(f)
	     (concat literef-arc-filter-variables-prefix f))
	   (list "in" "out" "depth"))))

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

(defun literef-subgraph-build-from-source()
  "Build the current subgraph based on its source. If the source type is :buffer, makes sure that the buffer exists. If not, offers to reset the subgraph."
  (interactive)
  (literef-make-arc-filter
   (literef-subgraph-source-property :filter-string))
  (let ((initial-keys (literef-subgraph-initial-keys)))
    (dolist (key initial-keys nil)
      (literef-subgraph-add-key key))
    (when (eq (literef-subgraph-source-property :source-type)
	      :buffer)
      (let ((buffer-node-name
	     (literef-subgraph-source-property :buffer-node-name)))
	(literef-subgraph-add-key buffer-node-name)
	(dolist (key initial-keys nil)
	  (literef-subgraph-add-generating-arc buffer-node-name key))))
    (literef-uniform-cost-search initial-keys)))

(defun literef-select-subgraph(&optional key)
  "Select subgraph of the graph of keys `literef-subgraph'. If KEY is specified, it is considered to be the currently active key to be used as the source."
  (interactive)
  (unwind-protect
      (progn
	(org-ref-cancel-link-messages)
	(setq current-subgraph (literef-init-subgraph))
	(literef-subgraph-select-source key)
	(when (eq (literef-subgraph-source-property :source-type)
		  :buffer)
	  (let* ((default-buffer-name
		   (concat "Buffer \"" (buffer-name) "\""))
		 (buffer-node-name
		  (read-string
		   (concat
		    "Enter the name of the buffer node. "
		    "This name will be used for the export as well"
		    " [" default-buffer-name "]: ")
		   nil nil default-buffer-name)))
	    (literef-subgraph-set-source-property
	     :buffer-node-name buffer-node-name)))
	(let ((filter-string
	       (literef-read-arc-filter
		"Enter a predicate for the arc filter: \n")))
	  (literef-subgraph-set-source-property
	   :filter-string filter-string)
	  (literef-subgraph-build-from-source)))
    (progn
      (org-ref-show-link-messages)
      (setq org-ref-show-citation-on-enter t)))
  (setq literef-subgraph current-subgraph)
  (literef-show-selected-subgraph)
  literef-subgraph)

(defun literef-subgraph-reset-selection()
  "Reset the subgraph selection to be the entrire graph."
  (interactive)
  (let ((current-subgraph (literef-init-subgraph)))
    (literef-subgraph-build-from-source)
    (setq literef-subgraph current-subgraph)))

(literef-subgraph-reset-selection)
(let ((current-subgraph literef-subgraph))
  (literef-subgraph-keys))

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

(defun literef-arc-filter-p()
  "Returns t if the current arc satisfies the filter and nil otherwise.

This function is constructed by `literef-make-arc-filter'.")

;; The default arc filter.
(literef-make-arc-filter "t")

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
  (scroll-down 1))

(defun literef-graph-scroll-down()
  "Handle scrolling left."
  (interactive)
  (scroll-up 1))

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

(defun literef-selected-subgraph-string()
  "Return a string representation of the current subgraph. Respects `literef-subgraph-show-only-generating-arcs' and `literef-subgraph-show-buffer'."
  (let ((res "graph { flow: south; }\n") ;; http://bloodgate.com/perl/graph/manual/hinting.html
	(buffer-node-name
	 (literef-subgraph-source-property :buffer-node-name))
	(keys (literef-subgraph-keys)))
    (dolist (key (literef-hash-keys-to-list keys) nil)
      (let ((out-neighbors
	     (if (eq key buffer-node-name)
		 (when literef-subgraph-show-buffer
		   (mapcar (lambda(k) (list k nil))
			   (literef-subgraph-initial-keys)))
	       (literef-hash-pairs-to-list
		(literef-key-out-neighbors key literef-graph)))))
	(dolist (out-neighbor out-neighbors nil)
	  (let ((to-key (elt out-neighbor 0)))
	  (when (and (literef-key-in-subgraph-p to-key)
		     (or (not literef-subgraph-show-only-generating-arcs)
			 (literef-generating-arc-p key to-key)))
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
		       " --> " "[ cite:" to-key  " ]\n")))))))
    res))

(defun literef-show-selected-subgraph()
  "Visualize the selected subgraph. Respects `literef-subgraph-show-only-generating-arcs' and `literef-subgraph-show-buffer'."
  (interactive)
  (save-selected-window
    (let ((current-subgraph literef-subgraph)
	  (temp-file-name (make-temp-file "foo")))
      (with-temp-file temp-file-name
	(insert (literef-selected-subgraph-string)))
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

(defun test()
  (interactive)
  (message "%d" (window-start)))


(provide 'literef-subgraph)
