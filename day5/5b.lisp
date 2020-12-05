(defun seat-conv (s)
  (parse-integer s :radix 2))

(defun s-to-bin (s)
  (substitute #\0 #\F (substitute #\0 #\L (substitute #\1 #\B (substitute #\1 #\R s)))))

(defun get-ids (l)
  (loop for seat in l collect (+ (* 8 (seat-conv (CAR seat))) (seat-conv (CADR seat)))))

(defun parse-input (l)
  (mapcar (lambda (n) (list (subseq (s-to-bin n) 0 7) (subseq (s-to-bin n) 7))) l))

;;; Makes a list from 0 to highest seat id then does a set difference of that with list of seat ids
;;; Returns in high-to-low, so the first element is the missing seat
(defun start ()
  (let ((ids (get-ids (parse-input (get-file "day5/day5.csv")))))
    (CAR (set-difference (loop for i from 0 to (apply 'max ids) collect i) ids))))
