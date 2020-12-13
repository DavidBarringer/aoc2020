;;; This time the "x"s are replaced with nil and the initial timestamp is discarded
(defun parse-input (l)
  (mapcar (lambda (s) (parse-integer s :junk-allowed t)) (split "," (CADR l))))

;;; When the offset is divided by the bus ID perfectly, the lcm of ID and inc is set to the increment
;;; When it isn't, the increment is added to the offset. The offset is also increased by 1 any time a constraint is met.
;;; Because inc is the lcm of all IDs that have had constraints met, the constraints will still be met when offset is increased.
;;; Because offset is increased by 1 when the current constraint is met, the subsequent bus will depart 1 minute after.
;;; I couldn't think of a better function name at the time of writing.
(defun contest (offset inc l)
  (cond ((null l) offset)
        ((null (CAR l)) (contest (+ offset 1) inc (CDR l)))
        ((= 0 (rem offset (CAR l))) (contest (+ offset 1) (lcm inc (CAR l)) (CDR l)))
        (t (contest (+ offset inc) inc l))))

;;; Because the returned offset has the 1 minute gaps for each bus, we take of the length of the bus list
(defun start ()
  (let ((input (parse-input (get-file "day13/day13.csv"))))
    (- (contest (CAR input) 1 input) (length input))))
