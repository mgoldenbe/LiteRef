(defun literef-request-filename()
  "Compute the name of the next request file."
  (concat literef-drop-directory "request." (number-to-string (float-time)) ".rqt"))

(defun literef-server-request(type data)
  "Submit request to the server. TYPE is either getPdf (in this case, DATA is the key for which pdf is required) or getBib (in this case, DATA is the search query)."
  (with-temp-file (literef-request-filename)
    (insert (concat type " " data))))

(provide 'literef-server)
