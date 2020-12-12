;;; Takes each line and changes it to a dotted pair (char . int)
(defun parse-input (l)
  (loop for s in l collect (cons (char (subseq s 0 1 ) 0) (parse-integer (subseq s 1)))))

;;; Goes through each step, checking each instruction. N,E,S,W modify x and y directly.
;;; Facing is defined as 3,2,1,0 for N,E,S,W respectively.
;;; L and R change the direction the ship is facing by adding/subtracting angle/90 then doing mod 4.
;;; F checks if the ship is facing N/S or E/W. If E/W, then subtract 1 to get 1/-1, multiply by the distance and add to x.
;;; If N/S, subtract 2 to get 1/-1, multiply by the distance and add to y.
(defun move-ship (x y facing steps)
  (cond ((null steps) (cons x y))
        ((eq #\N (CAAR steps)) (move-ship x (+ y (CDAR steps)) facing (CDR steps)))
        ((eq #\E (CAAR steps)) (move-ship (+ x (CDAR steps)) y facing (CDR steps)))
        ((eq #\S (CAAR steps)) (move-ship x (- y (CDAR steps)) facing (CDR steps)))
        ((eq #\W (CAAR steps)) (move-ship (- x (CDAR steps)) y facing (CDR steps)))
        ((eq #\L (CAAR steps)) (move-ship x y (mod (+ facing (/ (CDAR steps) 90)) 4) (CDR steps)))
        ((eq #\R (CAAR steps)) (move-ship x y (mod (- facing (/ (CDAR steps) 90)) 4) (CDR steps)))
        ((AND (eq #\F (CAAR steps)) (= 0 mod facing 2)) (move-ship (+ x (* (- facing 1) (CDAR steps))) y facing (CDR steps)))
        ((eq #\F (CAAR steps)) (move-ship x (+ y (* (- facing 2) (CDAR steps))) facing (CDR steps)))))

;;; Add the absolute values of the returned coordinate pair.
(defun start ()
  (let ((final (move-ship 0 0 2 (parse-input (get-file "day12/day12.csv")))))
    (+ (abs (CAR final)) (abs (CDR final)))))
