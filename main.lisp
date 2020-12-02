(ql:quickload "uiop")
(ql:quickload "cl-ppcre")
(use-package 'cl-ppcre)

;;; Function used to load data from a file, no other parsing happens here
(defun get-file (name)
  (uiop:read-file-lines name))

;;; Used to load a specific day and part (remember that part must be escaped)
(defun ld (day part)
  (setq s (format nil "day~D/~D~A.lisp" day day part))
  (load s))

;;; Benchmarking tools
;;; This function makes it pretty
(defun bench (func-name runs)
  (format nil "~Fms" (float (bench-fraction func-name runs))))

;;; This function runs file given amount of times and averages the real times
(defun bench-fraction (func-name runs)
  (/ (loop repeat runs sum (let ((t1 (get-internal-real-time)))
                            (funcall func-name)
                            (let ((t2 (get-internal-real-time)))
                              (/ (- t2 t1) internal-time-units-per-second))))
      runs))

(defun exec ()
  (princ "Enter the day you wish to run: ")
  (setq ans (write-to-string (read)))
  (load (concatenate 'string "day" ans "/" ans "a.lisp"))
  (setq a (format nil "~%~%~%~%~%Part A: ~D~%~%" (start)))
  (load (concatenate 'string "day" ans "/" ans "b.lisp"))
  (setq b (format nil "Part B: ~D~%~%~%" (start)))
  (princ (concatenate 'string a b))
  t)
