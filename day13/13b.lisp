(defun parse-input (l)
  (mapcar (lambda (s) (parse-integer s :junk-allowed t)) (split "," (CADR l))))

(defun contest (offset inc l)
  (cond ((null l) offset)
        ((null (CAR l)) (contest (+ offset 1) inc (CDR l)))
        ((= 0 (rem offset (CAR l))) (contest (+ offset 1) (* inc (CAR l)) (CDR l)))
        (t (contest (+ offset inc) inc l))))

(defun start ()
  (let ((input (parse-input (get-file "day13/day13.csv"))))
    (contest 1 1 input)))
