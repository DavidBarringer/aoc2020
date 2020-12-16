;;; Start by splitting on double newline for a list like: (fields your-ticket other-tickets)
;;; Take the fields and create a list of lists of dotted pairs of the ranges
;;; Take your-ticket and make it into a list of numbers
;;; Take the other tickets and them into a list of lists of numbers
;;; Combine these into one list.
(defun parse-input (l)
  (let ((types (split (format nil "~%~%") l)))
    (list (loop for c in (split (format nil "~%") (CAR types)) collect
            (loop for n in (split " " c) for r = (split "-" n) if (= 2 (length r)) collect (cons (parse-integer (CAR r)) (parse-integer (CADR r)))))
          (mapcar 'parse-integer (split "," (CADR (split (format nil "~%") (CADR types)))))
          (mapcar (lambda (o) (mapcar 'parse-integer (split "," o))) (CDR (split (format nil "~%") (CADDR types)))))))

;;; Takes a number and a list of dotted pairs, checks that n is between the numbers in at least one of the dotted pairs.
(defun check-valid (n &rest ranges)
  (notevery 'null (mapcar (lambda (r) (AND (>= n (CAR r)) (<= n (CDR r)))) ranges)))

;;; Removes every number from a ticket that passes at least one range check.
(defun check-ticket (ranges ticket)
  (remove-if (lambda (n) (notevery 'null (mapcar (lambda (r) (apply 'check-valid n r)) ranges))) ticket))

;;; For every ticket, gets the numbers that fail every check, collected into a single list.
(defun check-tickets (ranges tickets)
  (apply 'nconc (mapcar (lambda (tic) (check-ticket ranges tic)) tickets)))

;;; Sum the numbers that fail the checks in the other tickets.
(defun start ()
  (let ((input (parse-input (get-file-string "day16/day16.csv"))))
    (apply '+ (check-tickets (CAR input) (CADDR input)))))
