(defvar cols)

(defun parse-input (l)
  (setq cols (coerce (mapcar (lambda (s) (concatenate 'vector s)) l) 'vector)))

(defun make-empty-vector ()
  (make-array (length (aref cols 0)) :fill-pointer 0))

(defun map-col (y res changes)
  (cond ((AND (= y (length cols)) (NOT changes)) cols)
        ((= y (length cols)) (setq cols res) (map-col 0 (make-empty-vector) nil))
        (t (vector-push (CDR (map-row 0 y (make-empty-vector) nil)) res)
           (map-col (+ y 1) res (OR changes (CAR (map-row 0 y (make-empty-vector) nil)))))))

;;; Replace 4 with 5
(defun map-row (x y res changes)
  (cond ((= x (length (aref cols 0))) (cons changes res))
        ((AND (eq #\L (aref (aref cols y) x)) (= 0 (occupy-count x y)))
         (vector-push #\# res) (map-row (+ x 1) y res t))
        ((AND (eq #\# (aref (aref cols y) x)) (<= 5 (occupy-count x y)))
         (vector-push #\L res) (map-row (+ x 1) y res t))
        (t (vector-push (aref (aref cols y) x) res) (map-row (+ x 1) y res changes))))

;;; Checks that (x,y) is still within the bounds of the seat list
(defun out-of-bounds (x y)
  (OR (> 0 x) (> 0 y) (<= (length cols) y) (<= (length (aref cols 0)) x)))

;;; Moves (x,y) by the gradient, then checks if oob/seat. If floor then step again.
(defun step-until-seat (x-diff y-diff x y)
  (cond ((out-of-bounds x y) 0)
        ((eq #\# (aref (aref cols y) x)) 1)
        ((eq #\L (aref (aref cols y) x)) 0)
        (t (step-until-seat x-diff y-diff (+ x x-diff) (+ y y-diff)))))

;;; Makes a list of coordinates from (-1,-1) to (1,1), discarding (0,0)
(defun get-coords ()
  (loop for i from -1 to 1 nconc (loop for j from -1 to 1 if (NOT (AND (= i 0) (= j 0))) collect (cons i j))))

;;; Goes through each of the gradients make by get-coords, sums the result of stepping through each
(defun occupy-count (x y)
  (loop for c in (get-coords) sum (step-until-seat (CAR c) (CDR c) (+ (CAR c) x) (+ (CDR c) y))))

(defun start ()
  (parse-input (get-file "day11/day11.csv"))
  (apply '+ (mapcar (lambda (s) (count-if (lambda (e) (eq e #\#)) s)) (coerce (map-col 0 (make-empty-vector) nil) 'list))))
