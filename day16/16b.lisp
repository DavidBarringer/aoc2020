(defun parse-input (l)
  (let ((types (split (format nil "~%~%") l)))
    (list (loop for c in (split (format nil "~%") (CAR types)) collect
            (loop for n in (split " " c) for r = (split "-" n) if (= 2 (length r)) collect (cons (parse-integer (CAR r)) (parse-integer (CADR r)))))
          (mapcar 'parse-integer (split "," (CADR (split (format nil "~%") (CADR types)))))
          (mapcar (lambda (o) (mapcar 'parse-integer (split "," o))) (CDR (split (format nil "~%") (CADDR types)))))))

(defun check-valid (n &rest ranges)
  (notevery 'null (mapcar (lambda (r) (AND (>= n (CAR r)) (<= n (CDR r)))) ranges)))

;;; Differes from part a in that it returns false if any number fails in a ticket
(defun check-ticket (ranges ticket)
  (notevery 'null (mapcar (lambda (n) (every 'null (mapcar (lambda (r) (apply 'check-valid n r)) ranges))) ticket)))

;;; Keeps a ticket if all of its numbers pass the checks
(defun check-tickets (ranges tickets)
  (remove-if (lambda (tic) (check-ticket ranges tic)) tickets))

;;; For each number in each ticket, if the number falls in the range given, add it to the list
(defun check-range (range tickets)
  (loop for tic in tickets collect (loop for i from 0 to (- (length tic) 1) if (apply 'check-valid (nth i tic) range) collect i)))

;;; Recursive set-intersection because LISP only allows you to intersect 2 lists.
(defun r-intersection (current others)
  (if (null others) current (r-intersection (intersection current (CAR others)) (CDR others))))

;;; For each set of ranges in a field, get the set intersection of positions in the tickets that satisfy the ranges
(defun check-ranges (ranges tickets)
  (loop for r in ranges for res = (check-range r tickets) collect (r-intersection (CAR res) (CDR res))))

;;; Goes through each range, where only 1 ticket position satisfies it, remove that position from all other ranges.
;;; Repeat this until the list of position lists is reduced to a single list of correct position for each range.
(defun match-range (times res)
  (if (= 0 times) res (match-range (- times 1)
   (loop for i from 0 to (- (length res) 1) for r = (nth i res) if (AND (listp r) (= 1 (length r))) nconc
    (nconc (mapcar (lambda (s) (if (listp s) (remove (CAR r) s) s)) (subseq res 0 i))
           r
           (mapcar (lambda (s) (if (listp s) (remove (CAR r) s) s)) (subseq res (+ i 1))))))))

;;; Orders "your ticket" by the ranges, takes the first 6 elements, then multiplies them.
(defun start ()
  (let ((input (parse-input (get-file-string "day16/day16.csv"))))
    (let ((range-check (check-ranges (CAR input) (check-tickets (CAR input) (CADDR input)))))
      (apply '* (subseq (loop for n in (match-range (length range-check) range-check) collect (nth n (CADR input))) 0 6)))))
