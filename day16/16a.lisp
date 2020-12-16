;;; Start by splitting on double newline for a list like: (fields your-ticket other-tickets)
;;; Take the fields and create a list of lists of dotted pairs of the ranges
;;; Take your-ticket and make it into a list of numbers
;;; Take the other tickets and them into a list of lists of numbers
;;; Combine these into one list.
(defun parse-input (l)
  (let ((types (split (format nil "~%~%") l)))
    (list (loop for c in (split (format nil "~%") (CAR types)) collect
            (loop for n in (split " " c) for r = (split "-" n) if (= 2 (length r)) collect (cons (parse-integer (CAR r)) (parse-integer (CADR r)))))
          (loop for n in (split "," (CADR (split (format nil "~%") (CADR types)))) collect (parse-integer n))
          (loop for tic in (CDR (split (format nil "~%") (CADDR types))) collect (loop for n in (split "," tic) collect (parse-integer n))))))

;;; Takes a number and a list of dotted pairs, checks that n is between the numbers in at least one of the dotted pairs.
(defun check-valid (n &rest ranges)
  (some (lambda (b) (eq b t)) (loop for r in ranges collect (AND (>= n (CAR r)) (<= n (CDR r))))))

;;; For each number in a ticket, check that number is valid for the ranges, if it fails every check, it is added to the result.
(defun check-ticket (ranges ticket)
  (loop for n in ticket for res = (loop for r in ranges collect (apply 'check-valid n r)) if (every 'null res) collect n))

;;; For every ticket, check the validity of its numbers, collected into a single list.
(defun check-tickets (ranges tickets)
  (loop for tic in tickets nconc (check-ticket ranges tic)))

;;; Sum the numbers that fail the checks in the other tickets.
(defun start ()
  (let ((input (parse-input (get-file-string "day16/day16.csv"))))
    (apply '+ (check-tickets (CAR input) (CADDR input)))))
