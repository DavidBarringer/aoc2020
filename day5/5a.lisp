;;; Replaces F and L with 0, B and R with 1. There may be a better way to do this
(defun s-to-bin (s)
  (substitute #\0 #\F (substitute #\0 #\L (substitute #\1 #\B (substitute #\1 #\R s)))))

;;; Converts each seat from its binary string to a decimal int
(defun get-ids (l)
  (mapcar (lambda (s) (parse-integer s :radix 2)) l))

(defun start ()
  (apply 'max (get-ids (mapcar 's-to-bin (get-file "day5/day5.csv")))))
