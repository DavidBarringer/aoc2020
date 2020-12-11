(defvar cols)

;;; Takes list of strings, sets cols to a vector of vectors of characters
(defun parse-input (l)
  (setq cols (coerce (mapcar (lambda (s) (concatenate 'vector s)) l) 'vector)))

;;; Makes an empty vector with length = row length
(defun make-empty-vector ()
  (make-array (length (aref cols 0)) :fill-pointer 0))

;;; If we reach the end without any changes, return the result
;;; If we reach the end with changes, start again with the result as the new initial list
;;; Otherwise calculate current row, then move to next one
(defun map-col (y res changes)
  (cond ((AND (= y (length cols)) (NOT changes)) cols)
        ((= y (length cols)) (setq cols res) (map-col 0 (make-empty-vector) nil))
        (t (vector-push (CDR (map-row 0 y (make-empty-vector) nil)) res)
           (map-col (+ y 1) res (OR changes (CAR (map-row 0 y (make-empty-vector) nil)))))))

;;; If the seat changes, flag "changes" as t, then add change to result
;;; Otherwise add the same seat to result
(defun map-row (x y res changes)
  (cond ((= x (length (aref cols 0))) (cons changes res))
        ((AND (eq #\L (aref (aref cols y) x)) (= 0 (occupy-count x y)))
         (vector-push #\# res) (map-row (+ x 1) y res t))
        ((AND (eq #\# (aref (aref cols y) x)) (<= 4 (occupy-count x y)))
         (vector-push #\L res) (map-row (+ x 1) y res t))
        (t (vector-push (aref (aref cols y) x) res) (map-row (+ x 1) y res changes))))

;;; Makes coordinates from (-1,-1) to (1,1) if they are (0,0) or move (x,y) out of bounds, discard
;;; Then counts the number of #s seen
(defun occupy-count (x y )
  (loop for i from -1 to 1 sum
    (loop for j from -1 to 1 if (AND (NOT (AND (= i 0) (= j 0)))
                                     (AND (<= 0 (+ x i)) (<= 0 (+ y j)))
                                     (AND (> (length (aref cols 0)) (+ x i)) (> (length cols) (+ y j)))
                                     (eq #\# (aref (aref cols (+ y j)) (+ x i)))) count j)))

;;; Parse input, map until there are no changes, then count the number of #s on each row, then sum the rows
(defun start ()
  (parse-input (get-file "day11/day11.csv"))
  (apply '+ (mapcar (lambda (s) (count-if (lambda (e) (eq e #\#)) s)) (coerce (map-col 0 (make-empty-vector) nil) 'list))))
