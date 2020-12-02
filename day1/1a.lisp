;;; Loops through each element in the list, comparing it with each element after it.
;;; Returns a list with nils and a single solution as a dotted pair.
(defun check-sum (l)
  (if (= (length l) 1) nil
    (cons (loop for x in (CDR l)
            if (= 2020 (+ (CAR l) x)) append (cons (CAR l) x))
                                      (check-sum (CDR l)))))

(defun start ()
  (let ((l (CAR (remove nil (check-sum (mapcar 'parse-integer (get-file "day1/day1.csv")))))))
          (* (CAR l) (CDR l))))

;;; An optimised solution. Sorts into two lists (one low-to-high, one high-to-low) the first elements of each are added together.
;;; If the result is less than 2020, remove element from lth lists; if it is greater remove element from htl list.
;;; When the result is equal to 2020, a dotted pair list is returned
(defun check-sum-o (l1 l2)
  (cond ((< 2020 (+ (CAR l1) (CAR l2))) (check-sum-o l1 (CDR l2)))
        ((> 2020 (+ (CAR l1) (CAR l2))) (check-sum-o (CDR l1) l2))
        (t (cons (CAR l1) (CAR l2)))))

(defun start-o ()
  (let ((l (sort (mapcar 'parse-integer (get-file "day1/day1.csv")) #'<)))
        (let ((r (check-sum-o l (reverse l))))
          (* (CAR r) (CDR r)))))
