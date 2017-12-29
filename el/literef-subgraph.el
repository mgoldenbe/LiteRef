;;;; Selection of the subgraph.

(defvar literef-subgraph nil
  "The selected subgraph: hash of keys that make it up.")

(defvar literef-subgraph-generating-arcs nil
  "The arcs traversed to generate the selected subgraph: hash with keys of the form <key1><space><key2>.")

(defcustom literef-subgraph-show-only-generating-arcs nil
  "Determines whether only the generating arcs of the subgraph should be visualized.")

(defvar literef-subgraph-properties nil
  "The selected subgraph's properties.")

(defun literef-subgraph-add-key(key &optional subgraph)
  "Add KEY to SUBGRAPH. If SUBGRAPH is not specified, it is `literef-subgraph'."
  (unless subgraph (setq subgraph literef-subgraph))  
  (puthash key t subgraph))

(defun literef-key-in-subgraph-p(key &optional subgraph)
  "Checks whether KEY is in SUBGRAPH. If SUBGRAPH is not specified, it is `literef-subgraph'."
  (unless subgraph (setq subgraph literef-subgraph))  
  (gethash key subgraph))

(defun literef-subgraph-add-generating-arc
    (from-key to-key &optional subgraph)
  "Add an arc from FROM-KEY to TO-KEY that was traversed for generating SUBGRAPH. If SUBGRAPH is not specified, it is `literef-subgraph'."
  (unless subgraph (setq subgraph literef-subgraph))
  (puthash (concat from-key " " to-key) t literef-subgraph-generating-arcs))

(defun literef-generating-arc-p
    (from-key to-key &optional generating-arcs)
  "Checks whether the arc from FROM-KEY to TO-KEY is in GENERATING-ARCS. If GENERATING-ARCS is not specified, it is `literef-subgraph-generating-arcs'."
  (unless generating-arcs
    (setq generating-arcs literef-subgraph-generating-arcs))
  (gethash (concat from-key " " to-key) generating-arcs))

(defun literef-init-subgraph(&optional init-keys)
  "Return a subgraph using INIT-KEYS as the list of keys and return the new subgraph."
  (let ((res (make-hash-table :test 'equal)))
    (dolist (key init-keys res)
      (literef-graph-add-key key res))))

(literef-init-subgraph nil)

(defun literef-reset-subgraph-selection()
  "Reset the subgraph selection to be the entrire graph and to have the properties corresponding to this selection."
  (interactive)
  (setq literef-subgraph (literef-all-keys))
  (setq literef-subgraph-generating-arcs
	(make-hash-table :test 'equal))
  (setq literef-subgraph-properties
	(list
	 :source-type :all-keys
	 :source nil
	 :buffer-node-name nil
	 :filter-string "t"))
  literef-subgraph)

(literef-reset-subgraph-selection)

(defun literef-subgraph-select-source()
  "Select the source for forming the subgraph and set the :source-type and :source properties of `literef-subgraph-properties'."
  (let* ((current-key (literef-current-key))
	 (ans (literef-read-char
	       (concat "Choose the source of initial keys:  "
		       "All keys (a)  |  Current buffer (b)"
		       (when current-key
			 (concat "  |  " current-key "(c)")))
	       (if current-key '(?a ?b ?c) '(?a ?b)))))
    (cond ((eq ans ?a)
	   (literef-plist-put literef-subgraph-properties
			      :source-type :all-keys))
	  ((eq ans ?b)
	   (literef-plist-put literef-subgraph-properties
			      :source-type :buffer)
	   (literef-plist-put literef-subgraph-properties
			      :source (buffer-name)))
	  ((eq ans ?c)
	   (literef-plist-put literef-subgraph-properties
			      :source-type :current-key)
	   (literef-plist-put literef-subgraph-properties
			      :source current-key)))))

(defun literef-subgraph-initial-keys()
  "Compute initial keys for forming the subgraph based on the stored source."
  (let ((source-type
	 (plist-get literef-subgraph-properties :source-type))
	(source (plist-get literef-subgraph-properties :source)))
  (cond ((eq source-type :all-keys)
	 (literef-all-keys))
	((eq source-type :buffer)
	 (condition-case nil
	     (with-current-buffer source
	       (literef-hash-keys-to-list (literef-out-citations)))
	   (error (error (concat "The buffer " source " does not exist anymore. Consider running literef-reset-subgraph-selection to reset the subgraph selection.")))))
	((eq source-type :current-key)
	   (list source)))))

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
	(literef-subgraph-add-key to-key literef-subgraph))
      (let ((direction (plist-get to-properties :direction)))
	(if (eq direction :out)
	    (literef-subgraph-add-generating-arc
	     from-key to-key literef-subgraph)
	  (literef-subgraph-add-generating-arc
	   to-key from-key literef-subgraph))))))

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

(defun literef-subgraph-from-saved-properties()
  "Select subgraph of the graph of keys `literef-subgraph' based on the saved `literef-subgraph-properties'. If the source type is :buffer, makes sure that the buffer exists. If not, offers to reset the subgraph."
  (interactive)
  (literef-make-arc-filter
   (plist-get literef-subgraph-properties :filter-string))
  (let ((initial-keys (literef-subgraph-initial-keys)))
    (setq literef-subgraph (literef-init-subgraph initial-keys))
    (setq literef-subgraph-generating-arcs (make-hash-table :test 'equal))
    (when (eq (plist-get literef-subgraph-properties :source-type)
	      :buffer)
      (let ((buffer-node-name
	     (plist-get literef-subgraph-properties :buffer-node-name)))
	(literef-subgraph-add-key buffer-node-name literef-subgraph)
	(dolist (key initial-keys nil)
	  (literef-subgraph-add-generating-arc
	   buffer-node-name key literef-subgraph))))
    (literef-uniform-cost-search initial-keys))
  (literef-show-selected-subgraph)
  literef-subgraph)

(defun literef-select-subgraph()
  "Select subgraph of the graph of keys `literef-subgraph'."
  (interactive)
  (unwind-protect
      (progn
	(org-ref-cancel-link-messages)
	(literef-subgraph-select-source)
	(literef-plist-put literef-subgraph-properties
			   :buffer-node-name nil)
	(when (eq (plist-get literef-subgraph-properties :source-type)
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
	    (literef-plist-put literef-subgraph-properties
			       :buffer-node-name buffer-node-name)))
	(let ((initial-keys (literef-subgraph-initial-keys))
	      (filter-string
	       (literef-read-arc-filter
		"Enter a predicate for the arc filter: \n")))
	  (literef-plist-put literef-subgraph-properties
			     :filter-string filter-string)
	  (literef-subgraph-from-saved-properties)))
    (progn
      (org-ref-show-link-messages)
      (setq org-ref-show-citation-on-enter t)))
  literef-subgraph)

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

(defun literef-selected-subgraph-string()
  "Return a string representation of the selected subgraph. Respects `literef-subgraph-show-only-generating-arcs'."
  (let ((res "graph { flow: south; }\n") ;; http://bloodgate.com/perl/graph/manual/hinting.html
	(buffer-node-name
	 (plist-get literef-subgraph-properties :buffer-node-name)))
    (dolist (key (literef-hash-keys-to-list literef-subgraph) nil)
      (let ((out-neighbors
	     (if (eq key buffer-node-name)
		 (mapcar (lambda(k) (list k nil))
			 (literef-subgraph-initial-keys))
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
  "Visualize the selected subgraph. Respects `literef-subgraph-show-only-generating-arcs'."
  (interactive)
  (save-selected-window
    (let ((temp-file-name (make-temp-file "foo")))
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
