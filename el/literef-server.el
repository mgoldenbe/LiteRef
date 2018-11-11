;;; literef-server.el --- communication with the Python server.

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
;; This module contains functions that handle communication with the Python server.

;;; Code:
(defun literef-request-filename()
  "Compute the name of the next request file."
  (concat literef-drop-directory "request." (number-to-string (float-time)) ".rqt"))

(defun literef-server-request(type data)
  "Submit request to the server. TYPE is either \"getPdf\" or \"getBib\". If it is \"getPdf\", then DATA is the key for which pdf is required. If it is \"getBib\", then DATA is the search query."
  (with-temp-file (literef-request-filename)
    (insert (concat type " " data))))

(provide 'literef-server)
