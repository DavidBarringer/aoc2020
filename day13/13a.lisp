;;; Takes the input and makes a list with CAR initial timestamp and CDR bus IDs (removing "x"s)
(defun parse-input (l)
  (cons (parse-integer (CAR l)) (loop for s in (split "," (CADR l)) if (not (eq #\x (char s 0))) collect (parse-integer s))))

;;; Takes the remainder of initial timestamp/bus ID and subtracts it from the bus ID, giving time until the bus next arrives
(defun get-next-depart (depart l)
  (loop for bus in l collect (- bus (rem depart bus))))

;;; Multiplies the smallest time-to-bus and multiplies it by the ID at that position in the parsed input
(defun start ()
  (let ((input (parse-input (get-file "day13/day13.csv"))))
    (let ((next-bus-time (get-next-depart (CAR input) (CDR input))))
       (* (apply 'min next-bus-time) (nth (position (apply 'min next-bus-time) next-bus-time) (CDR input))))))
