;;; Does the same as part a, but 4D
(defun to-array (l)
  (make-array (list (length l) (length (CAR l)) (length (CAAR l)) (length (CAAAR l))) :initial-contents l))

;;; Does the same as part a, but 4D and cursed.
(defun add-padding-chars (l)
  (let ((pad-chars-w (make-list (+ 2 (length (CAR l)))
                                :initial-element (make-list (+ 2 (length (CAAR l)))
                                                            :initial-element (make-list (+ 2 (length (CAAAR l))) :initial-element #\.))))
        (pad-chars-z (make-list (+ 2 (length (CAAR l))) :initial-element (make-list (+ 2 (length (CAAAR l))) :initial-element #\.))))
    (nconc (list pad-chars-w)
           (loop for w in l collect
             (nconc (list pad-chars-z)
                    (loop for z in w collect
                      (nconc (list (make-list (+ 2 (length z)) :initial-element #\.))
                             (loop for y in z collect (nconc (list #\.) y (list #\.)))
                             (list (make-list (+ 2 (length z)) :initial-element #\.))))
                    (list pad-chars-z)))
           (list pad-chars-w))))

(defun parse-input (l)
 (to-array (add-padding-chars (list (list (mapcar (lambda (s) (concatenate 'list s)) l))))))

;;; Adds a 4-th dimension
(defun get-neighbours (x y z w initial)
  (loop for i from -1 to 1 sum
    (loop for j from -1 to 1 sum
      (loop for k from -1 to 1 sum
        (loop for l from -1 to 1 if (AND (NOT (AND (= 0 i) (= 0 j) (= 0 k) (= 0 l)))
                                         (<= 0 (+ x i)) (> (array-dimension initial 3) (+ x i))
                                         (<= 0 (+ y j)) (> (array-dimension initial 2) (+ y j))
                                         (<= 0 (+ z k)) (> (array-dimension initial 1) (+ z k))
                                         (<= 0 (+ w l)) (> (array-dimension initial 0) (+ w l))
                                         (eq #\# (aref initial (+ w l) (+ z k) (+ y j) (+ x i)))) count x)))))

(defun make-x (x y z w res initial)
  (cond ((= x (array-dimension initial 3)) res)
        ((AND (eq #\# (aref initial w z y x)) (OR (= 2 (get-neighbours x y z w initial)) (= 3 (get-neighbours x y z w initial))))
         (make-x (+ x 1) y z w (cons #\# res) initial))
        ((AND (eq #\. (aref initial w z y x)) (= 3 (get-neighbours x y z w initial)))
         (make-x (+ x 1) y z w (cons #\# res) initial))
        (t (make-x (+ x 1) y z w (cons #\. res) initial))))

(defun make-y (y z w res initial)
  (cond ((= y (array-dimension initial 2)) res)
        (t (make-y (+ y 1) z w (cons (make-x 0 y z w nil initial) res) initial))))

(defun make-z (z w res initial)
  (cond ((= z (array-dimension initial 1)) res)
        (t (make-z (+ z 1) w (cons (make-y 0 z w nil initial) res) initial))))

;;; Makes the list of lists of lists of lists of states
(defun make-w (times w res initial)
(cond ((= times 0) initial)
      ((= w (array-dimension initial 0)) (make-w (- times 1) 0 nil (to-array (add-padding-chars res))))
      (t (make-w times (+ w 1) (cons (make-z 0 w nil initial) res) initial))))

(defun count-hashes (cc)
  (loop for w from 1 to (- (array-dimension cc 0) 2) sum
    (loop for z below (array-dimension cc 1) sum
      (loop for y below (array-dimension cc 2) sum
        (loop for x below (array-dimension cc 3) if (eq #\# (aref cc w z y x)) count x)))))

(defun start ()
  (count-hashes (make-w 6 0 nil (parse-input (get-file "day17/day17.csv")))))
