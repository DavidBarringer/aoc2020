(defun parse-input (l)
  (mapcar 'parse-integer l))

(defun generate-arrangements (l)
  (if (= (length l) 1) 1
    (+ (generate-arrangements (CDR l)) (cond ((>= 3 (+ (CAR l) (CADR l))) (generate-arrangements (cons (+ (CAR l) (CADR l)) (CDDR l))))
                                             (t 0)))))
(defun to-comb (l)
  (loop for i in l collect (if (= 1 (length i)) (CAR i) (- (apply '+ i) 1))))

(defun count-valid (check val l)
  (cond ((null (CAR l)) val)
        ((>= 3 (- (CAR l) check)) (count-valid check (+ 1 val) (CDR l)))
        (t val)))

(defun arrangements (tot l)
  (cond ((null l) tot)
        ((> 4 (length l)) (cons (count-valid (CAR l) 0 (CDR l)) tot))
        (t (arrangements (cons (count-valid (CAR l) 0 (subseq l 1 4)) tot) (CDR l)))))

(defun split-on-ones (l clt res)
  (remove nil (cond ((null l) (cons clt res))
                    ((= (CAR l) 1) (split-on-ones (CDR l) nil (cons clt res)))
                    (t (split-on-ones (CDR l) (cons (CAR l) clt) res)))))

(defun start ()
  (let ((sort-list (cons 0 (sort (parse-input (get-file "day10/day10.csv")) '<))))
    (apply '* (to-comb (split-on-ones (arrangements nil sort-list) nil nil)))))
