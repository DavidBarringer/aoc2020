(defun check-sum (l)
  (if (= (length l) 1) nil (cons (loop for x in (CDR l) append (CAR (loop for y in (CDDR l) if (= 2020 (+ (CAR l) x y)) collect (list (CAR l) x y)))) (check-sum (CDR l)))))

(defun get-file (name)
  (mapcar 'parse-integer (uiop:read-file-lines name)))

(defun start ()
  (setq l (CAR (remove nil (check-sum (get-file "day1/day1.csv")))))
  (* (CAR l) (CADR l) (CADDR l)))
