(ql:quickload "uiop")
(ql:quickload "cl-ppcre")
(use-package 'cl-ppcre)

;;; Function used to load data from a file, no other parsing happens here
(defun get-file (name)
  (uiop:read-file-lines name))

;;; Used to load a specific day and part (remember that part must be a char)
(defun ld (day part)
  (setq s (format nil "day~D/~D~C.lisp" day day part))
  (load s))

(defun exec ()
  (princ "Enter the day you wish to run: ")
  (setq ans (write-to-string (read)))
  (load (concatenate 'string "day" ans "/" ans "a.lisp"))
  (setq a (format nil "~%~%~%~%~%Part A: ~D~%~%" (start)))
  (load (concatenate 'string "day" ans "/" ans "b.lisp"))
  (setq b (format nil "Part B: ~D~%~%~%" (start)))
  (princ (concatenate 'string a b))
  t)
