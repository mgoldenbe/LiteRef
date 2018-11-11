;;; literef.el --- the main module.

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
;; This module loads all the features of the system, starts
;; the server and performs some general setting up tasks.

;;; Code:

(defun literef-install-packages()
  "Install any missing packages. The code is taken from
`https://stackoverflow.com/a/10093312/2725810'."

  (setq package-list '(org org-ref pdf-tools smooth-scrolling company))

					; list the repositories containing them
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (package-initialize)

  (let (flag)
    (dolist (package package-list)
      (unless (eq (package-installed-p package) t) ;; need to take into account
	;; the built-in org package.
	(when 
	    (yes-or-no-p (concat
			  "The package " (symbol-name package)
			  " is not installed. Install it? "))
	  ;; Make sure the contents are refreshed once.
	  (unless flag
	    (package-refresh-contents)
	    (setq flag t))
	  ;; again, some what complicated to account for the built-in org
	  ;; see https://emacs.stackexchange.com/q/45936/16048
	  (let ((desc (car (cdr (assq package package-archive-contents)))))
	    (if desc
		(package-install desc 'dont-select)
	      (error (error "The package is unavailable. Something is wrong with the package system."))))
	  (when (eq package 'pdf-tools) (pdf-tools-install))))))
  (package-initialize)
  )

(literef-install-packages)

(require 'org)

(setq org-ref-completion-library 'org-ref-helm-cite)
(require 'org-ref)

(require 'bibtex-completion)
(require 'org-inlinetask)

(require 'literef-config)
(require 'literef-utils)
(require 'literef-helm)
(require 'literef-latex-map)
(require 'literef-citation-functions)
(require 'literef-graph)
(require 'literef-subgraph)
(require 'literef-export)
(require 'literef-pdf)
(require 'literef-server)
(require 'literef-citation-link)

;; start the server, while making sure that
;; only one instance of it is runnning.
(shell-command "pkill literef_server")
(call-process-shell-command
 (concat (file-name-directory load-file-name)
	 "py/literef_server.py" " " literef-directory "&") nil 0)
	   
;; advice org-ref-helm-insert-cite-link to begin by re-reading the default bibliography,
;; since entries could be added/removed.
;; (advice-add 'org-ref-helm-insert-cite-link :before #'literef-set-default-bibliography)
(advice-add 'org-ref-helm-cite :before #'literef-set-default-bibliography)

(defun literef-completion-fallback-candidates(_orig-fun)
  "The LiteRef version of `bibtex-completion-fallback-candidates'. It simply returns `bibtex-completion-fallback-options' without appending this list by all the BibTeX files."
  bibtex-completion-fallback-options)

;; Override `bibtex-completion-fallback-candidates'
(advice-add 'bibtex-completion-fallback-candidates :around #'literef-completion-fallback-candidates)

;; Turn off code evaluation security.
(setq org-confirm-babel-evaluate nil)

;; Set up pdflatex compilation.
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))


