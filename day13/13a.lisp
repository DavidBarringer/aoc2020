(defun parse-input (l)
  (cons (parse-integer (CAR l)) (loop for s in (split "," (CADR l)) if (not (eq #\x (char s 0))) collect (parse-integer s))))

(defun get-next-depart (depart l)
  (loop for bus in l collect (- bus (rem depart bus))))

(defun start ()
  (let ((input (parse-input (get-file "day13/day13.csv"))))
    (let ((next-bus-time (get-next-depart (CAR input) (CDR input))))
       (* (apply 'min next-bus-time) (nth (position (apply 'min next-bus-time) next-bus-time) (CDR input))))))
