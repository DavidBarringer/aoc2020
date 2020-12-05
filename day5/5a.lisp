;;; Changes a binary string to a decimal integer
(defun seat-conv (s)
  (parse-integer s :radix 2))

;;; Replaces F and L with 0, B and R with 1. There may be a better way to do this
(defun s-to-bin (s)
  (substitute #\0 #\F (substitute #\0 #\L (substitute #\1 #\B (substitute #\1 #\R s)))))

;;; Each seat is a list of row binary and col binary, converts and sums (multiplying row by 8 first)
(defun get-ids (seats)
  (loop for seat in seats collect (+ (* 8 (seat-conv (CAR seat))) (seat-conv (CADR seat)))))

;;; Changes the list of strings into binary, then splits into rows and cols
(defun parse-input (l)
  (mapcar (lambda (n) (list (subseq n 0 7) (subseq n 7))) (mapcar 's-to-bin l)))

(defun start ()
  (apply 'max (get-ids (parse-input (get-file "day5/day5.csv")))))
