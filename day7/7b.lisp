(defun parse-input (l)
  (loop for b in (mapcar (lambda (s) (split "contain" (remove #\. (remove #\  s)))) l)
    collect (cons (read-from-string (unpluralise (CAR b))) (list (pair-int (split "," (CADR b)))))))

(defun pair-int (l)
  (loop for s in l if (NOT (string= s "nootherbags"))
    collect (cons (read-from-string (unpluralise (remove (digit-char (parse-integer s :junk-allowed t)) s))) (parse-integer s :junk-allowed t))))

(defun unpluralise (s)
  (string-right-trim "s" s))

(defun find-bag-contains (target l)
  (if (null (CADR (nth (position target (mapcar 'car l)) l))) nil (CADR (nth (position target (mapcar 'car l)) l))))

(defun descend (c l)
  (cond ((null c) 1)
        (t (+ (descend (CDR c) l) (* (CDAR c) (descend (find-bag-contains (CAAR c) l) l))))))

(defun start ()
  (let ((input (parse-input (get-file "day7/day7.csv"))))
    (- (descend (find-bag-contains 'shinygoldbag input) input) 1)))
