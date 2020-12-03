(defun tree-check (x y map)
  (eq #\# (nth (mod x (length (CAR map))) (nth y map))))

;;; Modified to accommodate different gradients
(defun count-trees (map xdiff ydiff)
  (loop for i from 0 to (length map) count (tree-check (* xdiff i) (* ydiff i) map)))

(defun make-map (l)
  (mapcar (lambda (n) (concatenate 'list n)) l))

;;; Makes a list of the result for each run, then multiply them
(defun start()
  (let ((map (make-map (get-file "day3/day3.csv")))
        (runs ((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2))))
          (apply '* (loop for run in runs collect (count-trees map (CAR run) (CDR run))))))
