;;; literef-graph.el --- functions for building and updating the citation graph.

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
;; This module contains functions for both building the citation graph
;; from scratch and updating it dynamically upon changes in the
;; researcher's notes. The updates take place whenever notes
;; associated with a key are saved.

;;; Code:
(require 'smooth-scrolling)

;;;; Collecting citation links and functions from a buffer or from notes of a key.
(defun literef-add-citation-and-functions-to-hash
    (citation-link functions hash)
  "Add keys in CITATION-LINK and the citation FUNCTIONS to  HASH, in which keys are keys of BibTeX entries and values are sets of citation functions corresponding to the keys. Return the resulting hash."
  (dolist (key (literef-link-path-components citation-link) nil)
    (let ((functions-hash
	   (gethash key hash (make-hash-table :test 'equal))))
      (dolist (f functions nil)
	(puthash f t functions-hash))
      (puthash key functions-hash hash)))
  hash)

(defun literef-out-citations()
  "For the current buffer, compute a mapping, where keys are citations (i.e. keys of the BibTeX entries of the cited papers) and values are functions associated with each citation."
  (let ((buffer-string (buffer-string)))
    (with-temp-buffer
      (org-mode)
      (insert buffer-string)
      (org-export-expand-include-keyword)
      (let ((res (make-hash-table :test 'equal)))
	;; Handle citations that have annotation links.
	(dolist (annotation-link (literef-citation-function-links) nil)
	  (let ((citation-link
		 (literef-link-prev-element annotation-link)))
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
	   res))))))

(defun literef-key-out-citations(key)
  "Compute a mapping, where keys are citations in the notes associated with KEY and values are functions associated with each citation."
  (with-temp-buffer
    (insert-file-contents (literef-notes-filename key))
    (literef-out-citations)))

;; (defun literef-dump-out-citations()
;;   "Just for debugging of `literef-out-citations'. Create a list of citations with their functions for the current buffer."
;;   (interactive)
;;   (let ((res-hash (literef-out-citations))
;; 	(res))
;;     (dolist (pair
;; 	     (literef-hash-pairs-to-list res-hash)
;; 	     res)
;;       (push (cons (elt pair 0) (elt pair 1)) res))
;;     (setq res (reverse res))
;;     (message "%s" res))
;;   nil)

;;;; Building the citation graph with citations as arcs.
;;;; The building is done in a way that supports incremental updates.
;;;; Each arc is labeled with citation functions.
;;;; The vertices are stored in a hash table.
;;;; In and out-adjacency lists are hash tables.
;;;; In the out-adjacency, each key is mapped to a list of citation functions.
;;;; In the in-adjacency, each key is mapped to t. So, we do not store the citation functions twice and do not require syncronization of the two ends of an arc. However, this requires an extra look-up to find the list of citation functions for an in-neighbor.

(defun literef-graph-add-key(key &optional graph)
  "Add KEY to GRAPH. If GRAPH is not specified, it is given by the current value of the variable `literef-graph'."
  (unless graph (setq graph literef-graph))  
  (puthash
   key
   (cons
    (make-hash-table :test 'equal) ; out-adjacency
    (make-hash-table :test 'equal)) ; in-adjacency
   graph))

(defun literef-graph-add-arc(from-key to-key citation-functions
					       &optional graph)
  "Add an arc in GRAPH from FROM-KEY to TO-KEY labeled by CITATION-FUNCTIONS. If GRAPH is not specified, it is given by the current value of the variable `literef-graph'."
  (unless graph (setq graph literef-graph))
  (let ((out-neighbors (literef-key-out-neighbors from-key graph))
	(in-neighbors (literef-key-in-neighbors to-key graph)))
    (puthash to-key citation-functions out-neighbors)
    (puthash from-key t in-neighbors)))

(defun literef-init-graph(&optional init-keys)
  "Initialize the citation graph using INIT-KEYS as the list of keys. If the optional INIT-KEYS is not specified, all of the keys in the papers database are used."
  (unless init-keys (setq init-keys (literef-all-keys)))
  (let ((res (make-hash-table :test 'equal)))
    (dolist (key init-keys res)
      (literef-graph-add-key key res))))

(defun literef-graph-key-entry(key &optional no-error)
  "Return the set of arcs associated with KEY. If KEY is not in the citation graph, but is a valid key from the database, then add it to the citation graph. If KEY is not a valid key, then, throw fatal error instead, unless NO-ERROR is not nil, in which case nil is returned."
  (let ((res (gethash key graph)))
    (if res
	res
      (if (literef-key-exists key)
	  (literef-graph-add-key key graph)
	(error (error (concat "The key " key " is not in the database.")))))))

(defun literef-key-out-neighbors(key &optional graph)
  "Compute the out-neighbors of KEY in GRAPH. If GRAPH is not specified, it is given by the current value of the variable `literef-graph'. If KEY is not in GRAPH, it is added."
  (unless graph (setq graph literef-graph))
  (car (literef-graph-key-entry key)))

(defun literef-key-in-neighbors(key &optional graph)
  "Compute the in-neighbors of KEY in GRAPH. If GRAPH is not specified, it is given by the current value of the variable `literef-graph'. If KEY is not in GRAPH, it is added."
  (unless graph (setq graph literef-graph))
  (cdr (literef-graph-key-entry key)))

(defun literef-graph-arc-label(from-key to-key &optional graph)
  "Return the citation functions by which an arc in GRAPH from FROM-KEY to TO-KEY is labeled. If GRAPH is not specified, it is given by the current value of the variable `literef-graph'."
  (unless graph (setq graph literef-graph))
  (gethash to-key (literef-key-out-neighbors from-key graph)))

(defun literef-graph-update-key(key)
  "Update the citation graph based on the current version of the notes associated with KEY."
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
  "This hook is called to update the citation graph whenever the notes associated with a key are saved."
  (when (eq major-mode 'org-mode)
    (let ((key (literef-current-buffer-key)))
      (when key
	(literef-graph-update-key key)
	(message "The citation graph is updated. You may want to update the subgraph selection.")))))

(add-hook 'after-save-hook 'literef-save-hook)

(defun literef-compute-graph()
  "Compute the citation graph, with keys corresponding to all the papers in the papers database in the vertex set, from scratch."
  (interactive)
  (setq literef-graph (literef-init-graph))
  (dolist (key (literef-all-keys) literef-graph)
    (literef-graph-update-key key)))

(defvar literef-graph (literef-compute-graph)
  "The citation graph.")

(provide 'literef-graph)
