;;; Changes list of strings into list of ints
(defun parse-input (l)
  (mapcar 'parse-integer l))

;;; Makes a list of sums of pairs of numbers in the window of o to d
(defun generate (o d l)
  (loop for i from o to (- d 1) nconc
    (loop for j from (+ i 1) to (- d 1) collect (+ (nth i l) (nth j l)))))

;;; For each number in list starting from the offset, check if number appears in generated pairs
;;; between i and i - offset
(defun find-results (offset l)
  (loop for i from offset to (- (length l) 1) if (null (find (nth i l) (generate (- i offset) i l))) collect (nth i l)))

(defun start ()
  (CAR (find-results 25 (parse-input (get-file "day9/day9.csv")))))
