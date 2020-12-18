(defun parse-input (l)
  (mapcar (lambda (s) (read-from-string (concatenate 'string "(" s ")" ))) l))

;;; Same as part a, but if function symbol is *, then evaluate the left and right sides first.
(defun evaluate (l)
  (cond ((= 1 (length l)) (CAR l))
        ((listp (CAR l)) (evaluate (cons (evaluate (CAR l)) (CDR l))))
        ((listp (CADDR l)) (evaluate (nconc (list (CAR l) (CADR l) (evaluate (CADDR l))) (CDDDR l))))
        ((eql '* (CADR l)) (funcall (CADR l) (evaluate (list (CAR l))) (evaluate (CDDR l))))
        (t (evaluate (cons (funcall (CADR l) (CAR l) (CADDR l)) (CDDDR l))))))

(defun start ()
  (apply '+ (mapcar 'evaluate (parse-input (get-file "day18/day18.csv")))))
