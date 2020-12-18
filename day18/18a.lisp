;;; Takes any character, makes it a string then makes it into a value
(defun read-from-char (c)
  (read-from-string (format nil "~A" c)))

;;; Loop for inside of a parentheses. Collects the values and counts how many chars it passes.
;;; Returns a list of values inside the parentheses.
(defun collect-inner (chars res count)
  (cond ((eq #\) (CAR chars)) (cons (+ count 1) (reverse res)))
        ((eq #\( (CAR chars)) (collect-inner (nthcdr (CAR (collect-inner (CDR chars) nil 1)) chars)
                                             (cons (CDR (collect-inner (CDR chars) nil 1)) res)
                                             (+ count (CAR (collect-inner (CDR chars) nil 1)))))
        (t (collect-inner (CDR chars) (cons (read-from-char (CAR chars)) res) (+ count 1)))))

;;; Loop for collecting characters until "(" is reached.
;;; Then runs collect-inner to put characters inside parentheses into a list
(defun collect-parentheses (chars res)
  (cond ((eq #\( (CAR chars)) (collect-parentheses (nthcdr (CAR (collect-inner (CDR chars) nil 1)) chars) (cons (CDR (collect-inner (CDR chars) nil 1)) res)))
        ((null chars) (reverse res))
        (t (collect-parentheses (CDR chars) (cons (read-from-char (CAR chars)) res)))))

;;; Remove spaces from string, make into a list of characters, then parse using collect-parentheses
(defun parse-input (l)
  (mapcar (lambda (s) (collect-parentheses (concatenate 'list (remove #\Space s)) nil)) l))

;;; If left or right side of equation is a list, evaluate that first.
;;; Otherwise do function call on 2nd, 1st, and 3rd elements then add result to list
(defun evaluate (l)
  (cond ((= 1 (length l)) (CAR l))
        ((listp (CAR l)) (evaluate (cons (evaluate (CAR l)) (CDR l))))
        ((listp (CADDR l)) (evaluate (nconc (list (CAR l) (CADR l) (evaluate (CADDR l))) (CDDDR l))))
        (t (evaluate (cons (funcall (CADR l) (CAR l) (CADDR l)) (CDDDR l))))))

(defun start ()
  (apply '+ (mapcar 'evaluate (parse-input (get-file "day18/day18.csv")))))
