(defun s-to-bin (s)
  (parse-integer (substitute #\0 #\F (substitute #\0 #\L (substitute #\1 #\B (substitute #\1 #\R s)))) :radix 2))

;;; Makes a list from 0 to highest seat id then does a set difference of that with list of seat ids
;;; Returns in high-to-low, so the first element is the missing seat
(defun start ()
  (let ((ids (mapcar 's-to-bin (get-file "day5/day5.csv"))))
    (CAR (set-difference (loop for i from 0 to (apply 'max ids) collect i) ids))))
