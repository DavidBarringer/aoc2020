;;; Iterates through the list checking each item with each pair that appears after it for a sum.
;;; Returns a list with nils and two correct solutions in a single list.
;;; After removing the nils, the first 3 elements (i.e. a solution) are multiplied together.
(defun check-sum (l)
  (if (= (length l) 1) nil
                       (nconc (loop for x in (CDR l) nconc
                                (CAR (loop for y in (CDDR l) if (= 2020 (+ (CAR l) x y)) collect (list (CAR l) x y))))
                              (check-sum (CDR l)))))

(defun start ()
  (let ((l (check-sum (mapcar 'parse-integer (get-file "day1/day1.csv")))))
    (* (CAR l) (CADR l) (CADDR l))))

;;; These functions are an optimised solution, iterates through one list as normal, subtracting the item from 2020
;;; the other lists are sorted (one low-to-high, one high-to-low) sum the first element of each and compare to the result above.
;;; If it is less, remove element from lth list; if it is more, remove element from htl list.
;;; Returns a list with nils and 3 combinations of the correct solution.
(defun check-sum-o (l1 l2 l3)
  (cond ((OR (eq nil l1) (eq nil l2)) nil)
        ((< (- 2020 l3) (+ (CAR l1) (CAR l2))) (check-sum-o l1 (CDR l2) l3))
        ((> (- 2020 l3) (+ (CAR l1) (CAR l2))) (check-sum-o (CDR l1) l2 l3))
        (t (list (CAR l1) (CAR l2) l3))))

(defun start-o ()
  (let ((l (sort (mapcar 'parse-integer (get-file "day1/day1.csv")) #'<)))
    (let ((r (CAR (remove nil (loop for x in l collect (check-sum-o l (reverse l) x))))))
      (apply '* r))))
