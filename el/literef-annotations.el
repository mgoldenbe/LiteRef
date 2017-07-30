(defun literef-annotation-help-echo(window object position)
  (save-excursion
    (goto-char position)
    (goto-char (org-element-property :end (org-element-context)))
    (format "%s %s %s" "The current paper" (match-string 2) "the ideas of the cited one.")))
    
(org-link-set-parameters
 literef-annotation-link
 :follow (lambda (path) (message "%s %s %s" "The current paper" path "the ideas of the cited one."))
 :export (lambda (path desc backend) "") ; ignore the link
 :face `(:foreground ,literef-annotation-color)
 :help-echo 'literef-annotation-help-echo)

(defun literef-join-strings(strings separator)
  "Join the list STRINGS of strings putting the SEPARATOR string between them."
  (let ((remain (length strings))
	(res ""))
    (dolist (s strings res)
      (setq res (concat res s))
      (setq remain (1- remain))
      (when (> remain 0) (setq res (concat res separator))))))

(defun literef-annotate()
  "Annotate a citation with user-selected citation functions."
  (interactive)
  (let* ((link (org-element-context)))
    (if (literef-is-citation-link link)
	(let ((functions
	       (completing-read-multiple "Select or more comma-separated citation functions (press TAB for completion):\n" literef-citation-functions)))
	  (goto-char (literef-link-end link))
	  (insert " " literef-annotation-link ":" (literef-join-strings functions ",")))
      (progn
	(message "There is no citation link to annotate.")
	nil))))

(provide 'literef-annotations)
