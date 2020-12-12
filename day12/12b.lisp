(defun parse-input (l)
  (loop for s in l collect (cons (char (subseq s 0 1 ) 0) (parse-integer (subseq s 1)))))

;;; Modified from part a to have waypoint x and y, as well as ship x and y.
;;; N,E,S,W move the waypoint instead of the ship.
;;; If L or R and rotation is a multiple of 180, multiply waypoint x and y by 1 - rotation/90 mod 4
;;; Since rotation is a multiple of 180, this will give 1/-1.
;;; Otherwise, if L move the waypoint y to waypoint x, make it negative if rotation/90 mod 4 is 3.
;;; Also move waypoint x to waypoint y, make it negative if rotation/90 mod 4 is 1.
;;; Do the same for R, except swap the negative cases.
;;; If F, move the ship by the scaled x and y of the waypoint.
(defun move-ship (wx wy sx sy steps)
  (cond ((null steps) (cons sx sy))
        ((eq #\N (CAAR steps)) (move-ship wx (+ wy (CDAR steps)) sx sy (CDR steps)))
        ((eq #\E (CAAR steps)) (move-ship (+ wx (CDAR steps)) wy sx sy (CDR steps)))
        ((eq #\S (CAAR steps)) (move-ship wx (- wy (CDAR steps)) sx sy (CDR steps)))
        ((eq #\W (CAAR steps)) (move-ship (- wx (CDAR steps)) wy sx sy (CDR steps)))
        ((AND (OR (eq #\L (CAAR steps)) (eq #\R (CAAR steps))) (= 0 (mod (/ (CDAR steps) 90) 2)))
         (move-ship (* wx (- 1 (mod (/ (CDAR steps) 90) 4))) (* wy (- 1 (mod (/ (CDAR steps) 90) 4))) sx sy (CDR steps)))
        ((eq #\L (CAAR steps))
         (move-ship (* wy (- (mod (/ (CDAR steps) 90) 4) 2)) (* wx (- 2 (mod (/ (CDAR steps) 90) 4))) sx sy (CDR steps)))
        ((eq #\R (CAAR steps))
         (move-ship (* wy (- 2 (mod (/ (CDAR steps) 90) 4))) (* wx (- (mod (/ (CDAR steps) 90) 4) 2)) sx sy (CDR steps)))
        ((eq #\F (CAAR steps)) (move-ship wx wy (+ sx (* (CDAR steps) wx)) (+ sy (* (CDAR steps) wy)) (CDR steps)))))

(defun start ()
  (let ((final (move-ship 10 1 0 0 (parse-input (get-file "day12/day12.csv")))))
    (+ (abs (CAR final)) (abs (CDR final)))))
