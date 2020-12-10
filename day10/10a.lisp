(defun parse-input (l)
  (mapcar 'parse-integer l))

(defun get-diff (l)
  (let ((sl (sort l '<)))
   (mapcar '- sl (cons 0 sl))))

(defun start ()
  (let ((diff-list (get-diff (parse-input (get-file "day10/day10.csv")))))
    (* (count-if (lambda (n) (= 1 n)) diff-list) (+ 1 (count-if (lambda (n) (= 3 n)) diff-list)))))
