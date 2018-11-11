;;; literef-subgraph.el --- building and working with the citation subgraph. 

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
;; This module handles the whole process of building the citation
;; subgraph, beginning with specifying the source and the arc filter and
;; culminating in the uniform-cost search that builds the subgraph. It
;; also handles building the subgraphs for the source blocks inserted
;; into the researcher's notes by the wizard.
;; Lastly, it defines a minor mode for viewing the subgraph in a variety
;; of formats.

;;; Code:

;;;; Selection of the subgraph.

(defvar literef-subgraph nil
  "The selected subgraph with the following components: 

:keys -- the hash of keys in the subgraph.

:initial-keys -- the list of keys, which served as the root of the uniform-cost search that built the subgraph.

:generating-arcs -- the arcs that were traversed by the uniform-cost search that built the subgraph.

:source -- the source, based on which the subgraph was constructed. The source consists of:

    - :source-type -- the type of the source for building the graph. This can be `:all-keys', `:key' or `:buffer'.

    - :source-name -- the name of key or buffer.

    - :buffer-node-name -- the name to be used for the node corresponding to the buffer source in the visualization.

    - :file-name -- the file which the source buffer was visiting at the time of building the subgraph.

    - :filter-string -- the arcs filter that was used to construct the graph.")

(defun literef-subgraph-keys()
  "Return the keys component of the current subgraph."
  (plist-get current-subgraph :keys))

(defun literef-subgraph-initial-keys()
  "Return the initial keys component of the current subgraph."
  (plist-get current-subgraph :initial-keys))

(defun literef-subgraph-set-initial-keys(keys)
  "Set the initial keys component of the current subgraph to KEYS."
  (plist-put current-subgraph :initial-keys keys))

(defun literef-subgraph-generating-arcs()
  "Return the generating arcs component of the current subgraph."
  (plist-get current-subgraph :generating-arcs))

(defun literef-subgraph-source()
  "Return the source component of the current subgraph."
  (plist-get current-subgraph :source))

(defun literef-subgraph-source-property(property)
  "Return the value of the property PROPERTY of the source of current subgraph."
  (let ((source (literef-subgraph-source)))
    (plist-get source property)))

(defun literef-subgraph-set-source-property(property value)
  "Set the property PROPERTY of the source of current subgraph to VALUE."
  (let ((source (literef-subgraph-source)))
    (literef-plist-put source property value)))

(defun literef-subgraph-add-key(key)
  "Add KEY to the current-subgraph."
  (let ((keys (literef-subgraph-keys)))
    (puthash key t keys)))

(defun literef-key-in-subgraph-p(key)
  "Return t if KEY is in the current subgraph and nil otherwise."
  (let ((keys (literef-subgraph-keys)))
    (when (gethash key keys) t)))

(defun literef-subgraph-add-generating-arc(from-key to-key)
  "Add a generating arc from FROM-KEY to TO-KEY to the current subgraph."
  (let ((generating-arcs (literef-subgraph-generating-arcs)))
    (puthash (concat from-key " " to-key) t generating-arcs)))

(defun literef-generating-arc-p(from-key to-key)
  "Return t if the arc from FROM-KEY to TO-KEY is a generating arc of the current subgraph and nil otherwise."
  (let ((generating-arcs (literef-subgraph-generating-arcs)))
    (when (gethash (concat from-key " " to-key) generating-arcs) t)))

(defun literef-init-subgraph()
  "Make and return a new subgraph consisting of all the keys in the citation graph."
  (list :keys (make-hash-table :test 'equal)
	:generating-arcs (make-hash-table :test 'equal)
	:source (list
		 :source-type :all-keys
		 :source-name "survey"
		 :buffer-node-name nil
		 :file-name nil
		 :filter-string "t")))

(defun literef-subgraph-select-source(&optional key file-name)
  "Compute the source for forming the current subgraph based on the optional arguments, as follows. 

1. If KEY is specified, it becomes the source. Otherwise,

2. If some key is currently active, it becomes the source. Otherwise,

3. If FILE is specified and the file exists, it becomes the source. Otherwise,

4. The file returned by `buffer-file-name' becomes the source."
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
	(literef-plist-put source :source-name (buffer-name))
	(literef-plist-put
	 source :file-name
	 (let ((file (if file-name file-name (buffer-file-name))))
	   (when (and file (file-exists-p file))
	     (expand-file-name file))))))))
			     
(defun literef-subgraph-compute-initial-keys()
  "Compute the list of keys for forming the subgraph. All the keys
cited in the source are used. If the source is a buffer, it is assumed
to be currently active."
  (let ((source-type (literef-subgraph-source-property :source-type))
	(source-name (literef-subgraph-source-property :source-name)))
  (cond ((eq source-type :all-keys)
	 (literef-all-keys))
	((eq source-type :buffer)
	 (literef-hash-keys-to-list (literef-out-citations)))
	((eq source-type :current-key)
	   (list source-name)))))

(defun literef-arc-filter-temp-variable(str)
  "Return the symbol named literef-temp-<prefix>STR. The prefix is determined by the variable `literef-arc-filter-variables-prefix'."
  (intern (concat "literef-temp-"
		  literef-arc-filter-variables-prefix str)))

(defun literef-graph-check-arc(from-key to-key-cons)
  "Return t if the arc represented by FROM-KEY and TO-KEY-CONS (consisting of a key and a plist) fits the arc filter and nil otherwise."
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
  "Handle the arc represented by FROM-KEY and TO-KEY-CONS (consisting
of a key and a plist). If the arc fits the filter, then both the sink
key (i.e. the car component of TO-KEY-CONS) and the corresponding
generating arc are inserted in the current subgraph. If, in addition,
the duplicate detection is passed, then add the sink key to the keys
for the next iteration."
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

(defun literef-neighbor-pairs(direction cur-properties)
  "Compute the neighbors pairs consisting of key and properties that
fit the required DIRECTION of the arc. The properties are computed
based on CUR-PROPERTIES of the key being expanded. Properties include
depth, direction and citation functions."
  (let (res)
    (let ((neighbors
	   (literef-hash-pairs-to-list
	    (if (eq direction :out)
		(literef-key-out-neighbors key)
	      (literef-key-in-neighbors key)))))
      (dolist (pair neighbors res)
	(let ((properties (copy-seq cur-properties)))
	  (setq properties
		(plist-put properties :depth
			   (1+ (plist-get cur-properties :depth))))
	  (setq properties
		(plist-put properties :direction direction))
	  (setq properties
		(plist-put properties :functions
			   (if (eq direction :out)
			       (literef-graph-arc-label
				key (elt pair 0))
			     (literef-graph-arc-label
			      (elt pair 0) key))))
	  (push (cons (car pair) properties) res))))))

(defun literef-uniform-cost-search(initial-keys)
  "Build the current subgraph by performing uniform-cost search from
INITIAL-KEYS while respecting the current arc filter. The search keeps
direction. That is, if the current key was reached by following an
outgoing arc, then only the out-neighbors of that key will be
considered for the next iteration."
  (let ((cur-iter (make-hash-table :test 'equal))
	(next-iter (make-hash-table :test 'equal)))
    (dolist (key initial-keys nil)
      (puthash key
	       (list :direction nil :depth 0 :expanded nil) next-iter))
    (while (not (literef-hash-empty-p next-iter))
      (setq cur-iter next-iter)
      (setq next-iter (make-hash-table :test 'equal))
      (dolist (pair (literef-hash-pairs-to-list cur-iter) nil)
	(let ((key (elt pair 0))
	      (properties (elt pair 1)))
	  (remhash key cur-iter)
	  (setq properties (plist-put properties :expanded t))
	  (dolist (pair
		   (nconc
		    (literef-neighbor-pairs :out properties)
		    (literef-neighbor-pairs :in properties))
		   nil)
	    (literef-add-to-next-iter key pair)))))
    nil))

(defun literef-arc-filter-variables()
  "Return the list of variables recognized by the arc filter. Respects the variable `literef-arc-filter-variables-prefix'."
  (append
   (mapcar (lambda(f)
	     (concat literef-arc-filter-variables-prefix f))
	   literef-citation-functions)
   (mapcar (lambda(f)
	     (concat literef-arc-filter-variables-prefix f))
	   (list "in" "out" "depth"))))

(defun literef-make-arc-filter(str)
  "Form and evaluate the function `literef-arc-filter-p' corresponding
to the filter represented by the string STR."
  (with-temp-buffer
    (insert "(defun literef-arc-filter-p()\n")
    ;; (insert "\"Return t if the current arc satisfies the filter and nil otherwise. The implementation used for each particular building of the citation subgraph is formed by `literef-make-arc-filter'.\"\n")
    (insert "    (let (")
    (dolist (var-name (literef-arc-filter-variables) nil)
      (insert "\n          (" var-name " literef-temp-" var-name ")"))
    (insert ")\n       " str "))")
    ;; (message "Here:\n%s" (buffer-string)) 
    (eval-buffer)))

(defun literef-subgraph-build-from-source()
  "Build the current subgraph based on its source. If the source type is `:buffer', assumes that the source buffer is currently active."
  (interactive)
  (literef-make-arc-filter
   (literef-subgraph-source-property :filter-string))
  (literef-subgraph-set-initial-keys
   (literef-subgraph-compute-initial-keys))
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

(defun literef-select-subgraph-for-export
    (key-or-file filter buffer-node-name)
  "Select subgraph of the citation graph given by the value of the variable `literef-graph' for forming its visualization, with KEY-OR-FILE used as the source, FILTER used as the filter string and BUFFER-NODE-NAME used as the name of the buffer node in the visualization. In contrast to `literef-select-subgraph', this function is non-interactive. It is to be called from a source block inserted into the survey by the wizard."
  (let ((current-subgraph (literef-init-subgraph)))
    (when (not (or (literef-key-exists key-or-file)
		   (file-exists-p key-or-file)))
      (error (error (concat key-or-file " is neither key or file."))))
    (literef-subgraph-set-source-property :filter-string filter)
    (if (not (literef-key-exists key-or-file))
	(with-temp-buffer
	  (org-mode)
	  (insert-file-contents key-or-file)
	  (literef-subgraph-select-source nil key-or-file)
	  (literef-subgraph-set-source-property
	   :buffer-node-name buffer-node-name)
	  (literef-subgraph-build-from-source))
      (progn
	(literef-subgraph-select-source key-or-file)
	(literef-subgraph-build-from-source)))
    current-subgraph))

(defun literef-select-subgraph(&optional key)
  "Select subgraph of the citation graph given by the variable
`literef-graph'. If KEY is specified, it is considered to be the
currently active key to be used as the source. All the information
needed for building the subgraph, such as the arc filter, is requested
from the user."
  (interactive)
  (unwind-protect
      (progn
	(org-ref-cancel-link-messages)
	(setq current-subgraph (literef-init-subgraph))
	(literef-subgraph-select-source key)
	(when (eq (literef-subgraph-source-property :source-type)
		  :buffer)
	  (let* ((default-buffer-name (buffer-name))
		 (buffer-node-name
		  (if literef-subgraph-show-buffer
		      (read-string
		       "Enter the name of the buffer node: "
		       default-buffer-name)
		    default-buffer-name)))
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
  "Reset the subgraph selection to be the entire citation graph."
  (interactive)
  (let ((current-subgraph (literef-init-subgraph)))
    (literef-subgraph-build-from-source)
    (setq literef-subgraph current-subgraph)))

(literef-subgraph-reset-selection)
(let ((current-subgraph literef-subgraph))
  (literef-subgraph-keys))

;;;; Arc filter for subgraph selection.

(defun literef-list-satisfies-predicate-p(predicate list)
  "Return t if the LIST of citation functions satisfies the PREDICATE
and nil otherwise."
  (dolist (f list nil)
    (setq predicate
	  (literef-replace-in-string-whole-words f  "t" predicate)))
  (dolist (f literef-citation-functions nil)
    (setq predicate
	  (literef-replace-in-string-whole-words f "nil" predicate)))
  (condition-case nil
      (literef-eval-string predicate)
    (error (error "Could not evaluate: %s" predicate))))

(defun literef-arc-filter-company-backend
    (command &optional arg &rest ignored)
  "The company-mode back-end for entering the filter with completion
for citation functions. The technique is described at
`http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend'."  
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
  "Turn on company mode."
  (company-mode 1))

(defun literef-read-arc-filter(prompt)
  "Read the string representing an arc filter, while providing
completion of citation functions. The recognized citation functions
are given by the value of the variable
`literef-citation-functions'. Prompt the user with the string PROMPT."
  (let ((company-backends (copy-sequence company-backends))
	(minibuffer-local-map (copy-sequence minibuffer-local-map))
	(minibuffer-setup-hook (copy-sequence minibuffer-setup-hook))
	(resize-mini-windows t))
    (add-to-list 'company-backends 'literef-arc-filter-company-backend)
    (define-key minibuffer-local-map (kbd "TAB") 'literef-arc-filter-company-backend)
    (add-hook 'minibuffer-setup-hook 'literef-arc-filter-minibuffer-mode)
    (read-from-minibuffer prompt)))

(defun literef-arc-filter-p()
  "Return t if the current arc satisfies the filter and nil otherwise. This is a dummy implementation. The implementation used for each particular building of the citation subgraph is formed by `literef-make-arc-filter'."
  nil)

;; The default arc filter.
(literef-make-arc-filter "t")

;;;; Operations on the selected subgraph.

(defvar literef-graph-mode-map nil
  "The key map for the minor mode for viewing the selected subgraph `literef-graph-mode'.")

(defun literef-graph-scroll-right()
  "Handle scrolling right."
  (interactive)
  (scroll-left 1))

(defun literef-graph-scroll-left()
  "Handle scrolling left."
  (interactive)
  (scroll-right 1))

(defun literef-graph-scroll-up()
  "Handle scrolling up."
  (interactive)
  (scroll-down 1))

(defun literef-graph-scroll-down()
  "Handle scrolling down."
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
  "The definition of the minor mode for viewing the selected
subgraph."
  nil " LiteRefGraph" literef-graph-mode-map)  

(defun literef-selected-subgraph-string(format)
  "Return a string representation of the current subgraph in the given FORMAT. 

For \"txt\" or \"boxart\" FORMAT, keys and labels are links. For non-clickable formats, such as \"png\", links are not used to save space. 

The function respects the values of the variable `literef-subgraph-show-only-generating-arcs' and the variable `literef-subgraph-show-buffer'."
  (let ((res "graph { flow: south; }\n") ;; http://bloodgate.com/perl/graph/manual/hinting.html
	(textual-format (member format '("ascii" "boxart")))
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
		       (when (and textual-format
				  (literef-key-exists key)) "cite:")
		       key " ]"
		       (let ((functions
			      (mapcar
			       (lambda(f)
				 (concat
				  (when textual-format
				    (concat
				     literef-citation-function-link ":"))
				    f))
			       (elt out-neighbor 1))))
			 (if functions
			     (concat
			      " -- "
			      (literef-join-strings functions "\\n"))
			   ""))
		       " --> " "[ "
		       (when textual-format "cite:") to-key  " ]\n")))))))
    res))

(defun literef-subgraph-save-image(format)
  "Save the visualization of the selected subgraph using FORMAT, such as \"boxart\" or \"png\" in a temporary file and return the file's name. Respects the values of the variable `literef-subgraph-show-only-generating-arcs' and the variable `literef-subgraph-show-buffer'."
  (interactive)
  (save-selected-window
    (let ((current-subgraph literef-subgraph)
	  (description-file-name
	   (concat
	    (make-temp-file "literef-subgraph-description-") ".txt"))
	  (image-file-name
	   (concat (make-temp-file "literef-subgraph-image-")
		   "." format)))
      (with-temp-file description-file-name
	(insert (literef-selected-subgraph-string format)))
      (let* ((command (concat "graph-easy --as " format
			      " --output " image-file-name " "
			      description-file-name))
	     (output (shell-command-to-string command)))
	(message output))
      image-file-name)))

(defun literef-show-selected-subgraph-raw(format)
  "Compute the visualization of the selected subgraph using FORMAT,
  such as \"boxart\" or \"png\" and show it in a buffer. Respects the
  value of the variable `literef-subgraph-show-only-generating-arcs'
  and the variable `literef-subgraph-show-buffer'."
  (save-selected-window
    (let* ((buffer-name "The graph of keys.")
	   (last-buffer (get-buffer buffer-name))
	   (textual-format (member format '("ascii" "boxart")))
	   (current-subgraph literef-subgraph)
	   (image-file-name (literef-subgraph-save-image format)))
      (if last-buffer
	  (progn
	    (let ((window (get-buffer-window last-buffer))) 
	      (if window
		  (select-window window)
		(set-buffer last-buffer)))
	    (kill-buffer last-buffer)
	    (find-file image-file-name))
	(find-file-other-window image-file-name))
      (rename-buffer buffer-name)
      (when textual-format
	(org-mode)
	(literef-graph-mode 1)
	(setq-local auto-hscroll-mode nil)
	(smooth-scrolling-mode 1)
	
	;; Do not wrap lines
	(setq-local truncate-lines t)
	
	;; The following settings are for ASCII output. For BoxArt, they are not needed, but are still fine.
	;; Do not highlight as in tabular mode.
	;; https://emacs.stackexchange.com/a/13955/16048
	(setq-local org-enable-table-editor nil)
	(setq-local face-remapping-alist
		    (cons '(org-table . default) face-remapping-alist))
	    ;; Do not use strike-through.
	    ;; https://stackoverflow.com/a/22493885/2725810
	(setq-local org-emphasis-alist nil)

	;; Make sure that the font is monospace.
	(face-remap-add-relative 'default
				 :family "Monospace"
				 :height literef-graph-font-height)
	
	;; Smooth horizontal scrolling.
	(setq-local hscroll-margin 0)
	(setq-local hscroll-step 1)
	;; Add spaces, so horizontal scrolling is available in all lines.
	(literef-append-spaces (literef-longest-line-length))

	;; Reset position and scrolling.
	(beginning-of-buffer)
	(set-window-hscroll (selected-window) 0)
	(set-window-vscroll (selected-window) 0)

	;; Save buffer, so wouldn't be prompted afterwards
	(basic-save-buffer)
	
	;; Make the buffer read-only.
	(setq-local buffer-read-only t)
	)))
    nil)

(defun literef-show-selected-subgraph()
  "Visualize the selected subgraph. Respects `literef-subgraph-show-only-generating-arcs' and `literef-subgraph-show-buffer'."
  (interactive)
  (literef-show-selected-subgraph-raw "boxart"))

(defun literef-show-selected-subgraph-png()
  "Compute the visualization of the selected subgraph using the \"png\" format and show it in a buffer. Respects the
  value of the variable `literef-subgraph-show-only-generating-arcs'
  and the variable `literef-subgraph-show-buffer'."
  (interactive)
  (literef-show-selected-subgraph-raw "png"))

(defun literef-longest-line-length()
  "Compute the length of the longest line in the current buffer."
  (save-excursion
    (let ((res 0))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((cur-len (- (line-end-position) (line-beginning-position))))
	  (setq res (max res cur-len)))
	(forward-line))
      res)))

(defun literef-append-spaces(required-length)
  "Append spaces to all lines in the current buffer, so they become at
least the given REQUIRED-LENGTH long."
  (save-excursion
    (let ((res 0))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((cur-len (- (line-end-position) (line-beginning-position))))
	  (goto-char (line-end-position))
	  (insert (make-string (- required-length cur-len) ?\s)))
	(forward-line))
      nil)))

;; (defun test()
;;   (interactive)
;;   (message "%d" (window-start)))


(provide 'literef-subgraph)
