;;; Adds parentheses around the string, then reads the string to make a list of operations
(defun parse-input (l)
  (mapcar (lambda (s) (read-from-string (concatenate 'string "(" s ")" ))) l))

;;; If left or right side of equation is a list, evaluate that first.
;;; Otherwise do function call on 2nd, 1st, and 3rd elements then add result to list
(defun evaluate (l)
  (cond ((= 1 (length l)) (CAR l))
        ((listp (CAR l)) (evaluate (cons (evaluate (CAR l)) (CDR l))))
        ((listp (CADDR l)) (evaluate (nconc (list (CAR l) (CADR l) (evaluate (CADDR l))) (CDDDR l))))
        (t (evaluate (cons (funcall (CADR l) (CAR l) (CADDR l)) (CDDDR l))))))

(defun start ()
  (apply '+ (mapcar 'evaluate (parse-input (get-file "day18/day18.csv")))))
