(ql:quickload "uiop")
(ql:quickload "cl-ppcre")
(use-package 'cl-ppcre)

;;; Function used to load data from a file, seperated by new line, no other parsing happens here
(defun get-file (name)
  (uiop:read-file-lines name))

;;; Function used to load data from a file as a single string
(defun get-file-string (name)
  (remove #\Return (uiop:read-file-string name)))

;;; Used to load a specific day and part (remember that part must be escaped)
(defun ld (day part)
  (let ((s (format nil "day~D/~D~A.lisp" day day part)))
    (load s)))

;;; Benchmarking tools
;;; This function makes it pretty
(defun bench (func-name runs &optional args)
  (format nil "~Fs" (float (bench-fraction func-name runs args))))

;;; This function runs file given amount of times and averages the real times
(defun bench-fraction (func-name runs &optional args)
  (/ (loop repeat runs sum (let ((t1 (get-internal-real-time)))
                                  (apply func-name args)
                                  (let ((t2 (get-internal-real-time)))
                                          (/ (- t2 t1) internal-time-units-per-second))))
      runs))

(defun exec ()
  (princ "Enter the day you wish to run: ")
  (let ((ans (write-to-string (read))))
          (load (concatenate 'string "day" ans "/" ans "a.lisp"))
          (let ((a (format nil "~%~40,,,'*A ~%Part A: ~D~%~%" "" (start))))
                  (load (concatenate 'string "day" ans "/" ans "b.lisp"))
                  (let ((b (format nil "Part B: ~D~%~40,,,'*A~%~%" (start) "")))
                          (princ (concatenate 'string a b)) t))))
