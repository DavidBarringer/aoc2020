(defvar memory)

(defun parse-input (s)
  (let ((nums (split "," (remove #\Newline s))))
    (loop for i from 0 to (- (length nums) 1) collect (setf (gethash (parse-integer (nth i nums)) memory) i))))

(defun get-next (num it)
  (cond ((= it 29999999) num)
        ((null (gethash num memory)) (setf (gethash num memory) it) (get-next 0 (+ it 1)))
        (t (let ((next (- it (gethash num memory))))
             (setf (gethash num memory) it) (get-next next (+ it 1))))))

(defun start ()
  (setq memory (make-hash-table))
  (parse-input (get-file-string "day15/day15.csv"))
  (get-next 0 (hash-table-count memory)))
