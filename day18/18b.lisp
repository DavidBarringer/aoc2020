(defun read-from-char (c)
  (read-from-string (format nil "~A" c)))

(defun collect-inner (chars res count)
  (cond ((eq #\) (CAR chars)) (cons (+ count 1) (reverse res)))
        ((eq #\( (CAR chars)) (collect-inner (nthcdr (CAR (collect-inner (CDR chars) nil 1)) chars)
                                             (cons (CDR (collect-inner (CDR chars) nil 1)) res)
                                             (+ count (CAR (collect-inner (CDR chars) nil 1)))))
        (t (collect-inner (CDR chars) (cons (read-from-char (CAR chars)) res) (+ count 1)))))

(defun collect-parentheses (chars res)
  (cond ((eq #\( (CAR chars)) (collect-parentheses (nthcdr (CAR (collect-inner (CDR chars) nil 1)) chars) (cons (CDR (collect-inner (CDR chars) nil 1)) res)))
        ((null chars) (reverse res))
        (t (collect-parentheses (CDR chars) (cons (read-from-char (CAR chars)) res)))))

(defun parse-input (l)
  (mapcar (lambda (s) (collect-parentheses (concatenate 'list (remove #\Space s)) nil)) l))

;;; Same as part a, but if function symbol is *, then evaluate the left and right sides first.
(defun evaluate (l)
  (cond ((= 1 (length l)) (CAR l))
        ((listp (CAR l)) (evaluate (cons (evaluate (CAR l)) (CDR l))))
        ((listp (CADDR l)) (evaluate (nconc (list (CAR l) (CADR l) (evaluate (CADDR l))) (CDDDR l))))
        ((eql '* (CADR l)) (funcall (CADR l) (evaluate (list (CAR l))) (evaluate (CDDR l))))
        (t (evaluate (cons (funcall (CADR l) (CAR l) (CADDR l)) (CDDDR l))))))

(defun start ()
  (apply '+ (mapcar 'evaluate (parse-input (get-file "day18/day18.csv")))))
