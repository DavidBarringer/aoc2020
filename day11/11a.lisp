;;; Takes list of strings, returns list of lists of characters
(defun parse-input (l)
  (mapcar (lambda (s) (concatenate 'list s)) l))

;;; If we reach the end without any changes, return the result
;;; If we reach the end with changes, start again with the result as the new initial list
;;; Otherwise calculate current row, then move to next one
(defun map-col (y initial res changes)
  (cond ((AND (= y (length initial)) (NOT changes)) res)
        ((= y (length initial)) (map-col 0 res nil nil))
        (t (map-col (+ y 1) initial (cons (CDR (map-row 0 y initial nil nil)) res) (OR changes (CAR (map-row 0 y initial nil nil)))))))

;;; If the seat changes, flag "changes" as t, then add change to result
;;; Otherwise add the same seat to result
(defun map-row (x y initial res changes)
  (cond ((= x (length (CAR initial))) (cons changes res))
        ((AND (eq #\L (nth x (nth y initial))) (= 0 (occupy-count x y initial)))
         (map-row (+ x 1) y initial (cons #\# res) t))
        ((AND (eq #\# (nth x (nth y initial))) (<= 4 (occupy-count x y initial)))
         (map-row (+ x 1) y initial (cons #\L res) t))
        (t (map-row (+ x 1) y initial (cons (nth x (nth y initial)) res) changes))))

;;; Makes coordinates from (-1,-1) to (1,1) if the are (0,0) or move (x,y) out of bounds, discard
;;; Then counts the number of #s seen
(defun occupy-count (x y initial)
  (loop for i from -1 to 1 sum
    (loop for j from -1 to 1 if (AND (NOT (AND (= i 0) (= j 0)))
                                     (AND (<= 0 (+ x i)) (<= 0 (+ y j)))
                                     (eq #\# (nth (+ x i) (nth (+ y j) initial)))) count j)))

;;; Parse input, map until there are no changes, then count the number of #s on each row, then sum the rows
(defun start ()
  (apply '+ (mapcar (lambda (s) (count-if (lambda (e) (eq e #\#)) s)) (map-col 0 (parse-input (get-file "day11/day11.csv")) nil nil))))
