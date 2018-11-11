;;; literef-citation-functions.el --- implementation of the citation function link and the associated functionality.

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
;; This module defines the citation functions link and functions for
;; computing its help echo and for providing a convenient completing
;; read interface for entering the citation functions.

;;; Code:
(require 'company)
(require 'cl-lib)

(defun literef-citation-function-help-echo(_window _object position)
  "Compute the help echo that appears when hovering over a citation
function link. This particular implementation only uses POSITION of
the mouse. The technique is described at
`http://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/#org1c56ce9'." 
  (save-excursion
    (goto-char position)
    (goto-char (org-element-property :end (org-element-context)))
    (format "%s%s%s" "The current paper relates to the cited one(s) as: " (match-string 2) ".")))
    
(org-link-set-parameters
 literef-citation-function-link
 :follow (lambda (path) (message "%s%s%s" "The current paper relates to the cited one as: " path "."))
 :export (lambda (path desc backend) "") ; ignore the link
 :face `(:foreground ,literef-citation-function-color)
 :help-echo 'literef-citation-function-help-echo)

(defun literef-input-functions()
  "Prompt the user to input citation functions using the completing
read interface. Returns the list of selected citation functions."
  (completing-read-multiple "Select or more comma-separated citation functions (press TAB for completion):\n" literef-citation-functions))

(defun literef-citation-function()
  "Annotate the currently active citation link with user-selected
citation functions."
  (interactive)
  (let ((link (org-element-context)))
    (if (literef-citation-link-p link)
	(progn
	  (goto-char (literef-link-end link))
	  (insert " " literef-citation-function-link ":"
		  (literef-join-strings (literef-input-functions) ",")))
      (message "There is no citation link to annotate."))))

(provide 'literef-citation-functions)


