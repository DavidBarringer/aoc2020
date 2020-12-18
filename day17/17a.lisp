;;; Changes a list into an array so solution doesn't O(heat-death-of-the-universe)
(defun to-array (l)
  (make-array (list (length l) (length (CAR l)) (length (CAAR l))) :initial-contents l))

;;; Adds inactive states at every +/- 1 for each coordinate, so it is sized properly for the next iteration
(defun add-padding-chars (l)
  (let ((pad-chars (make-list (+ 2 (length (CAR l))) :initial-element (make-list (+ 2 (length (CAAR l))) :initial-element #\.))))
    (nconc (list pad-chars)
           (loop for z in l collect
             (nconc (list (make-list (+ 2 (length z)) :initial-element #\.))
                    (loop for y in z collect (nconc (list #\.) y (list #\.)))
                    (list (make-list (+ 2 (length z)) :initial-element #\.))))
           (list pad-chars))))

;;; Takes list of strings puts them into list of list of list of chars to be processed by the functions above
(defun parse-input (l)
 (to-array (add-padding-chars (list (mapcar (lambda (s) (concatenate 'list s)) l)))))

;;; Naive check for each neighbour, goes through the +/- 1 of each coordinate, checks it's in bounds then counts if it's active
(defun get-neighbours (x y z initial)
  (loop for i from -1 to 1 sum
    (loop for j from -1 to 1 sum
      (loop for k from -1 to 1 if (AND (NOT (AND (= 0 i) (= 0 j) (= 0 k)))
                                       (<= 0 (+ x i)) (> (array-dimension initial 2) (+ x i))
                                       (<= 0 (+ y j)) (> (array-dimension initial 1) (+ y j))
                                       (<= 0 (+ z k)) (> (array-dimension initial 0) (+ z k))
                                       (eq #\# (aref initial (+ z k) (+ y j) (+ x i)))) count x))))

;;; Builds the list of states based of their neighbours in the initial state
(defun make-x (x y z res initial)
  (cond ((= x (array-dimension initial 2)) res)
        ((AND (eq #\# (aref initial z y x)) (OR (= 2 (get-neighbours x y z initial)) (= 3 (get-neighbours x y z initial))))
         (make-x (+ x 1) y z (cons #\# res) initial))
        ((AND (eq #\. (aref initial z y x)) (= 3 (get-neighbours x y z initial)))
         (make-x (+ x 1) y z (cons #\# res) initial))
        (t (make-x (+ x 1) y z (cons #\. res) initial))))

;;; Builds the list of lists of states
(defun make-y (y z res initial)
  (cond ((= y (array-dimension initial 1)) res)
        (t (make-y (+ y 1) z (cons (make-x 0 y z nil initial) res) initial))))

;;; Builds the list of lists of lists of states
(defun make-z (times z res initial)
  (cond ((= times 0) initial)
        ((= z (array-dimension initial 0)) (make-z (- times 1) 0 nil (to-array (add-padding-chars res))))
        (t (make-z times (+ z 1) (cons (make-y 0 z nil initial) res) initial))))

;;; Goes through the entire 3D array and counts active states
(defun count-hashes (cc)
  (loop for z from 1 to (- (array-dimension cc 0) 2) sum
    (loop for y below (array-dimension cc 1) sum
      (loop for x below (array-dimension cc 2) if (eq #\# (aref cc z y x)) count x))))

(defun start ()
  (count-hashes (make-z 6 0 nil (parse-input (get-file "day17/day17.csv")))))
