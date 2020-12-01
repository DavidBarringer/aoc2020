(defun check-sum (l)
  (if (= (length l) 1) nil (cons (loop for x in (CDR l) if (= 2020 (+ (CAR l) x)) append (cons (CAR l) x)) (check-sum (CDR l)))))

(defun get-file (name)
  (mapcar 'parse-integer (uiop:read-file-lines name)))

(defun start ()
  (setq l (CAR (remove nil (check-sum (get-file "day1/day1.csv")))))
  (* (CAR l) (CDR l)))
