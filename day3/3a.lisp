;;; Uses mod to repeat thee pattern in x direction
(defun tree-check (x y map)
  (eq #\# (nth (mod x (length (CAR map))) (nth y map))))

;;; Moves a step and counts if it encounters a tree
(defun count-trees (map)
  (loop for i from 0 to (length map) count (tree-check (* 3 i) i map)))

;;; Changes the map from a list of strings to a list of char lists
(defun make-map (l)
  (mapcar (lambda (n) (concatenate 'list n)) l))

(defun start()
  (count-trees (make-map (get-file "day3/day3.csv"))))
