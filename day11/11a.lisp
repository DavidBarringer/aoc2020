(defun parse-input (l)
  (mapcar (lambda (s) (concatenate 'list s)) l))

(defun map-col (y initial res changes)
  (cond ((AND (= y (length initial)) (= changes 0)) (reverse res))
        ((= y (length initial)) (map-col 0 (reverse res) nil 0))
        (t (map-col (+ y 1) initial (cons (reverse (CDR (map-row 0 y initial nil 0))) res) (+ changes (CAR (map-row 0 y initial nil 0)))))))

(defun map-row (x y initial res changes)
  (cond ((= x (length (CAR initial))) (cons changes res))
        ((AND (eq #\L (nth x (nth y initial))) (= 0 (occupy-count x y initial)))
         (map-row (+ x 1) y initial (cons #\# res) (+ changes 1)))
        ((AND (eq #\# (nth x (nth y initial))) (<= 4 (occupy-count x y initial)))
         (map-row (+ x 1) y initial (cons #\L res) (+ changes 1)))
        (t (map-row (+ x 1) y initial (cons (nth x (nth y initial)) res) changes))))

(defun occupy-count (x y initial)
  (loop for i from -1 to 1 sum
    (loop for j from -1 to 1 if (AND (NOT (AND (= i 0) (= j 0)))
                                     (AND (<= 0 (+ x i)) (<= 0 (+ y j)))
                                     (eq #\# (nth (+ x i) (nth (+ y j) initial)))) count j)))

(defun start ()
  (apply '+ (mapcar (lambda (s) (count-if (lambda (e) (eq e #\#)) s)) (map-col 0 (parse-input (get-file "day11/day11.csv")) nil 0))))
