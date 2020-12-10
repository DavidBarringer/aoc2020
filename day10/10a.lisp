;;; List of strings to list of ints etc.
(defun parse-input (l)
  (mapcar 'parse-integer l))

;;; Subtract the list of jolts inluding start from the list of jolts.
;;; This means that each jolt is subtracted from the one after it.
(defun get-diff (l)
  (let ((sl (sort l '<)))
   (mapcar '- sl (cons 0 sl))))

;;; Since the final jolt isn't included, add it to the count of 3s before multiplying
(defun start ()
  (let ((diff-list (get-diff (parse-input (get-file "day10/day10.csv")))))
    (* (count-if (lambda (n) (= 1 n)) diff-list) (+ 1 (count-if (lambda (n) (= 3 n)) diff-list)))))
