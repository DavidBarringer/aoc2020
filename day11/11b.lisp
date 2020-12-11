(defun parse-input (l)
  (mapcar (lambda (s) (concatenate 'list s)) l))

(defun map-col (y initial res changes)
  (cond ((AND (= y (length initial)) (NOT changes)) res)
        ((= y (length initial)) (map-col 0 res nil nil))
        (t (map-col (+ y 1) initial (cons (CDR (map-row 0 y initial nil nil)) res) (OR changes (CAR (map-row 0 y initial nil nil)))))))

;;; Replace 4 with 5
(defun map-row (x y initial res changes)
  (cond ((= x (length (CAR initial))) (cons changes res))
        ((AND (eq #\L (nth x (nth y initial))) (= 0 (occupy-count x y initial)))
         (map-row (+ x 1) y initial (cons #\# res) t))
        ((AND (eq #\# (nth x (nth y initial))) (<= 5 (occupy-count x y initial)))
         (map-row (+ x 1) y initial (cons #\L res) t))
        (t (map-row (+ x 1) y initial (cons (nth x (nth y initial)) res) changes))))

;;; Checks that (x,y) is still within the bounds of the seat list
(defun out-of-bounds (x y initial)
  (OR (> 0 x) (> 0 y) (<= (length initial) y) (<= (length (CAR initial)) x)))

;;; Moves (x,y) by the gradient, then checks if oob/seat. If floor then step again.
(defun step-until-seat (x-diff y-diff x y initial)
  (cond ((out-of-bounds x y initial) 0)
        ((eq #\# (nth x (nth y initial))) 1)
        ((eq #\L (nth x (nth y initial))) 0)
        (t (step-until-seat x-diff y-diff (+ x x-diff) (+ y y-diff) initial))))

;;; Makes a list of coordinates from (-1,-1) to (1,1), discarding (0,0)
(defun get-coords ()
  (loop for i from -1 to 1 nconc (loop for j from -1 to 1 if (NOT (AND (= i 0) (= j 0))) collect (cons i j))))

;;; Goes through each of the gradients make by get-coords, sums the result of stepping through each
(defun occupy-count (x y initial)
  (loop for c in (get-coords) sum (step-until-seat (CAR c) (CDR c) (+ (CAR c) x) (+ (CDR c) y) initial)))

(defun start ()
  (apply '+ (mapcar (lambda (s) (count-if (lambda (e) (eq e #\#)) s)) (map-col 0 (parse-input (get-file "day11/day11.csv")) nil nil))))
