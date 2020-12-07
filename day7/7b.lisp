(defun parse-input (l)
  (loop for b in (mapcar (lambda (s) (split "contain" (remove #\. (remove #\  s)))) l)
    collect (cons (read-from-string (unpluralise (CAR b))) (list (pair-int (split "," (CADR b)))))))

(defun pair-int (l)
  (loop for s in l if (NOT (string= s "nootherbags"))
    collect (cons (read-from-string (unpluralise (remove (digit-char (parse-integer s :junk-allowed t)) s))) (parse-integer s :junk-allowed t))))

(defun unpluralise (s)
  (string-right-trim "bags" s))

;;; Function that returns what a given bag contains
(defun find-bag-contains (target l)
  (CADR (nth (position target (mapcar 'car l)) l)))

;;; Recursive function for totalling the number of bags in a bag.
;;; Basically a DFS that multiplies when taking a path and sums when all paths at level are exhausted
(defun descend (c l)
  (if (null c) 1 (+ (descend (CDR c) l) (* (CDAR c) (descend (find-bag-contains (CAAR c) l) l)))))

;;; Descend adds 1 when all contained bags are counted (because the 1 is also used for multiplication)
;;; The 1 gets taken away at the end
(defun start ()
  (let ((input (parse-input (get-file "day7/day7.csv"))))
    (- (descend (find-bag-contains 'shinygold input) input) 1)))
