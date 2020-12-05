(defun s-to-bin (s)
  (substitute #\0 #\F (substitute #\0 #\L (substitute #\1 #\B (substitute #\1 #\R s)))))

(defun get-ids (l)
  (mapcar (lambda (s) (parse-integer s :radix 2)) l))

;;; Makes a list from 0 to highest seat id then does a set difference of that with list of seat ids
;;; Returns in high-to-low, so the first element is the missing seat
(defun start ()
  (let ((ids (get-ids (mapcar 's-to-bin (get-file "day5/day5.csv")))))
    (CAR (set-difference (loop for i from 0 to (apply 'max ids) collect i) ids))))
