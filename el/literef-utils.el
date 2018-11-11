;;; literef-utils.el --- general-purpose utilities used by the other modules.

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
;; This module contains some helper functions. Some of these, such as
;; `literef-xor', are not related to LiteRef functionalities per se
;; and can be used by other projects. Others, such as
;; `literef-bib-files' are LiteRef-specific, but are used by many
;; modules and hence are separated out as utilities.

;;; Code:
(require 'literef-util-links)

(defun literef-xor(a b)
  "Compute A xor B."
  (not (eq a b)))

(defun literef-number-or-nil-p(string)
  "Return t if STRING represents a number and nil otherwise."
  (let ((converted (string-to-number string)))
    (if (= converted 0)
	(if (or (string= string "0") (string= string "nil"))  t nil)
      t)))

(defun literef-number-or-nil(string)
  "Convert STRING to number. Return nil if STRING does not represent a number."
  (if (string= string "nil") nil (string-to-number string)))

(defun literef-read-number-or-nil(prompt default)
  "Read either a number or \"nil\" from the user and return it."
  (let ((res "error"))
    (while (not (literef-number-or-nil-p res))
      (setq res (read-string prompt nil nil default)))
    (floor (literef-number-or-nil res))))

(defun literef-string-or-nil-to-string(string)
  "If STRING is nil, return \"nil\", otherwise return STRING."
  (if string
      string
    "nil"))

(defun replace-in-string (what with in)
  "Replace each occurrence of WHAT in the string IN with WITH. The code is taken from `https://stackoverflow.com/a/17325791/2725810'." 
  (let (case-fold-search)
    (replace-regexp-in-string (regexp-quote what) with in nil 'literal)))

(defun literef-replace-in-string-whole-words(what with in)
  "Like `replace-in-string', but replaces whole words. The code is taken from `https://emacs.stackexchange.com/a/34665/16048'."
  (replace-regexp-in-string (concat "\\b" what "\\b")  with in))

(defun literef-raw-str-compare(s1 s2)
  "Compare strings S1 and S2. Return -1 if S1 is smaller than S2, 1 if S2 is smaller than S1 and 0 if the strings are equal."
  (cond
   ((string< s1 s2) -1)
   ((string< s2 s1) 1)
   (t 0)))

(defun literef-join-strings(strings separator)
  "Join the list STRINGS of strings putting the SEPARATOR string between them."
  (let ((remain (length strings))
	(res ""))
    (dolist (s strings res)
      (setq res (concat res s))
      (setq remain (1- remain))
      (when (> remain 0) (setq res (concat res separator))))))

(defun literef-read-char(prompt legal-chars)
  "Read a char until one of the chars in LEGAL-CHARS is entered. Return the last read char. Prompt the user with PROMPT."
  (catch 'ok
    (while t
      (let ((ans (read-char prompt)))
      (when (member ans legal-chars) (throw 'ok ans))))))

(defun literef-word-correct-p(word)
  "Return t if WORD is spelled correctly and nil otherwise. Adapted from `flyspell-correct-word-before-point'."
  ;; (unless ispell-dictionary-alist
  ;;   (add-to-list 'ispell-dictionary-alist
  ;; 		 '("english" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en") nil utf-8))
  ;;   (ispell-change-dictionary "english"))
  (let (poss ispell-filter)
    (ispell-send-string "%\n")	;put in verbose mode
    (ispell-send-string (concat "^" word "\n"))
    ;; wait until ispell has processed word
    (while (progn
	     (accept-process-output ispell-process)
	     (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
	(setq ispell-filter '(*)))
    (if (consp ispell-filter)
	(setq poss (ispell-parse-output (car ispell-filter))))
    (cond
     ((or (eq poss t) (stringp poss))
      ;; The word is correct.
      t)
     ((null poss)
      ;; ispell error
      (error "Ispell: error in Ispell process"))
     (t
      ;; The word is incorrect.
      nil))))

(defun literef-eval-string (string)
  "Evaluate Emacs Lisp code given by STRING. The code is taken from `https://emacs.stackexchange.com/a/19878/16048'."
  (eval (car (read-from-string string))))

(defun literef-hash-empty-p (hash)
  "Return t if HASH is empty and nil otherwise."
  (eq (hash-table-count hash) 0))

(defun literef-plist-put(plist prop val)
  "Just a shortcut for `(setq plist (plist-put PLIST PROP VAL)).'"
  (setq plist (plist-put plist prop val)))

;; Based on: http://ergoemacs.org/emacs/elisp_hash_table.html
(defun literef-hash-keys-to-list (hash)
  "Return a list of keys in HASH."
  (if hash
      (let (res)
	(maphash (lambda (k _v) (push k res)) hash)
	res)
    nil))

(defun literef-hash-pairs-to-list (hash)
  "Return a list of key-value pairs in HASH."
  (if hash
      (let (res)
	(maphash (lambda (k v) (push (list k v) res)) hash)
	res)
    nil))

(defun literef-hash-keys-minus(hash1 hash2)
  "Return a list of keys that are present in HASH1, but not in HASH2."
  (let (res)
    (dolist (key (literef-hash-keys-to-list hash1) res)
      (unless (gethash key hash2 nil) (push key res)))))

(defun literef-buffer-keys()
  "Compute the list of all keys cited in the current buffer, sorted and with duplicates removed."
  (let (res)
    (dolist (link (literef-citation-links) nil)
      (dolist (key (literef-link-path-components link) nil)
	(setq res (cons key res))))
    (delete-dups (sort res 'string<))))

(defun literef-all-keys()
  "Compute the list of all keys in the papers database."
  (let (res)
    (dolist (key (directory-files literef-papers-directory) nil)
      (when (literef-key-exists key) (push key res)))
    (sort res 'string<)))

(defun literef-bib-files (&optional _arg)
  "Compute the list of BibTeX files in the papers database."
  (sort (file-expand-wildcards
	 (concat literef-papers-directory "*/paper.bib")) 'string<))

(defun literef-set-default-bibliography(&optional _orig-fun)
  "Set `org-ref-default-bibliography' to be the list of all the BibTeX files in the papers database."
  (setq org-ref-default-bibliography (literef-bib-files)))

;;;; BEGIN: Constant-time look-up of entries --------------------
(defun literef-get-entry (orig-fun &rest args)
  "The version of `bibtex-completion-get-entry1' with the only source set to the bib file corresponding to the given key, which is the entry-key argument contained in ARGS."
  (let ((bibtex-completion-bibliography (list (literef-bib-filename entry-key))))
    (apply orig-fun args)))

(advice-add 'bibtex-completion-get-entry1 :around #'literef-get-entry)
;;;; END --------------------------------------------------------

;;;; BEGIN: Opening various files associated with a paper -------
(defun literef-get-bibtex-key-under-cursor()
  "Non-throwing version of `org-ref-get-bibtex-key-under-cursor'. This version does not affect the current point in the buffer."
  (save-excursion
    (condition-case nil (org-ref-get-bibtex-key-under-cursor) (error nil))))

(defun literef-current-key()
  "Return the key at point. If there is no key under the cursor, then return the key whose associated file is being visited by the current buffer. If the current buffer is not visiting a file associated with a paper, return nil."
  (or (literef-get-bibtex-key-under-cursor)
      (literef-current-buffer-key)))

(defun literef-key-exists(key)
  "Return t if KEY exists and nil otherwise."
  (let ((filename (literef-bib-filename key)))
    (file-exists-p filename)))

(defun literef-filename(key ext)
  "Compute the name of a file with the extension EXT pertaining to a paper whose BibTeX entry's key is KEY. Note that this file does not need to exist."
  (concat literef-papers-directory key (concat "/paper." ext)))

(defun literef-bib-filename(key)
  "Compute the name of the BibTeX file based on KEY."
  (literef-filename key "bib"))

(defun literef-creation-timestamp(key)
  "Compute the creation timestamp of the database entry corresponding to KEY as a floating-point number of seconds. We use the modification timestamp of the BibTeX file, since, in general, the creation timestamp might not be stored by the operating system and accessing the creation date for ext4 is not trivial."
  (let* ((bibfile-name (literef-bib-filename key))
	 (attribute (elt (file-attributes bibfile-name) 5)))
    (time-to-seconds attribute))) 

(defun literef-notes-filename(key)
  "Compute name of the notes file based on KEY."
  (literef-filename key "org"))

(defun literef-pdf-filename(key)
  "Compute name of the PDF file based on KEY."
  (literef-filename key "pdf"))

(defun literef-find-file-other-window(filename)
  "The version of `find-file-other-window' that does not do anything if the file is already being visited in the current window."
  (if (equal (buffer-file-name) filename)
      (current-buffer)
    (find-file-other-window filename)))

(defun literef-open-key-notes(key)
  "Open the researcher's notes for KEY."
  (let ((filename (literef-notes-filename key)))
    (find-file-other-window filename)))

(defun literef-open-notes()
  "Open the researcher's notes for the current key. See `literef-current-key'."
  (interactive)
  (let ((key (literef-current-key)))
    (literef-open-key-notes key)))

(defun literef-open-key-bibfile(key)
  "Open the BibTeX file for KEY."
  (let ((filename (literef-bib-filename key)))
    (find-file-other-window filename)))

(defun literef-open-bibfile()
  "Open the BibTeX file for the current key. See `literef-current-key'."
  (interactive)
  (let ((key (literef-current-key)))
    (literef-open-key-bibfile key)))

;;;; Opening PDF

(defvar literef-needed-pdfs (make-hash-table :test 'equal)
  "Keys whose PDFs are currently being searched for in the online sources.")

(defun literef-check-arrived-pdfs()
  "Open PDFs for the keys contained in the list given by the value of the variable `literef-needed-pdfs'."
  (dolist (key (literef-hash-keys-to-list literef-needed-pdfs) nil)
    (let ((filename (literef-pdf-filename key)))
      (when (file-exists-p filename)
	(remhash key literef-needed-pdfs)
	(switch-to-buffer (find-file-other-window filename))))))

(cancel-function-timers 'literef-check-arrived-pdfs)
(run-with-idle-timer 0.1 t 'literef-check-arrived-pdfs)

(defun literef-open-key-pdf-raw(key)
  "Open the PDF for KEY. This function should not be used directly."
  (let ((filename (literef-pdf-filename key)))
    (when (file-exists-p filename)
      (switch-to-buffer (literef-find-file-other-window filename)))))

(defun literef-open-key-pdf(key)
  "Open the PDF for KEY."
  (unwind-protect
      (progn
	(org-ref-cancel-link-messages)
	(when (and
	       (gethash key literef-needed-pdfs nil)
	       (y-or-n-p "The PDF has already been requested. Would you like to request it again?"))
	  (remhash key literef-needed-pdfs))
	(org-ref-show-link-messages)))
  (let ((filename (literef-pdf-filename key)))
    (unless (or (file-exists-p filename)
		(gethash key literef-needed-pdfs nil))
      (literef-server-request "getPdf" key)))
  (puthash key t literef-needed-pdfs))
      
(defun literef-open-pdf()
  "Open the PDF for the current key. See `literef-current-key'."
  (interactive)
  (let ((key (literef-current-key)))
    (when key
      (literef-open-key-pdf key))))

;;;; Compute key based on folder, file or buffer

(defun literef-folder-key(folder)
  "Compute key based on a paper's FOLDER in the papers database. If FOLDER does not correspond to a paper, return nil."

  (let ((key (car (last (nbutlast (split-string folder "/"))))))
    (if (file-directory-p (concat literef-papers-directory key))
	key
      nil)))

(defun literef-file-key(filename)
  "Compute key based on the name FILENAME of one of the files associated with a paper. If the file is not associated with a paper, return nil."
  (when filename
    (literef-folder-key (file-name-directory filename))))

(defun literef-buffer-key(buffer)
  "Compute key based on the BUFFER visiting a file associated with a paper. If the buffer is not visiting a file associated with a paper, return nil."
  (literef-file-key (buffer-file-name buffer)))

(defun literef-current-buffer-key()
  "Compute key of the paper whose associated file is being visited by
the current buffer. If the current buffer is not visiting a file
associated with a paper, return nil."
  (literef-buffer-key (current-buffer)))

(defmacro with-cloned-buffer(&rest body)
  "Executes BODY just like `progn' but maintain the original buffer state. The code is taken from `https://emacs.stackexchange.com/a/31763/16048'."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let ((buffer-file-name nil))
       (clone-buffer nil t)
       (let ((,return-value (progn ,@body)))
	 (kill-buffer-and-window)
	 ,return-value))))

(provide 'literef-utils)
