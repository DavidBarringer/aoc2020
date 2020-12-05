;;; Replaces F and L with 0, B and R with 1, then parses the binary string into a decimal integer.
(defun s-to-bin (s)
  (parse-integer (substitute #\0 #\F (substitute #\0 #\L (substitute #\1 #\B (substitute #\1 #\R s)))) :radix 2))

(defun start ()
  (apply 'max (mapcar 's-to-bin (get-file "day5/day5.csv"))))
