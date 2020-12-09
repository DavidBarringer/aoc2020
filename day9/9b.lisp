(defun parse-input (l)
  (mapcar 'parse-integer l))

(defun generate (o d l)
  (loop for i from o to (- d 1) nconc
    (loop for j from (+ i 1) to (- d 1) collect (+ (nth i l) (nth j l)))))

;;; Hyper brute-force solution. Makes every subsequence from start to position of part a.
;;; If the answer to part a is equal to the sum of that subsequence, get the sum of the
;;; max and min values of the subsequence
(defun contig-sum (check l)
  (loop for i from 0 to check nconc
    (loop for j from (+ 1 i) to check for sub = (subseq l i j) if (= (apply '+ sub) (nth check l))
      collect (+ (apply 'max sub) (apply 'min sub)))))

;;; Same as part a, but calls contig sum with the position, rather than collecting the answer
(defun find-results (l offset)
  (contig-sum (CAR (loop for i from offset to (- (length l) 1) if (null (find (nth i l) (generate i l (- i offset)))) collect i)) l))

(defun start ()
  (CAR (find-results (parse-input (get-file "day9/day9.csv")) 25)))
