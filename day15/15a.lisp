(defvar memory)

;;; Split on , populate the hash table with :key number :value index in list
(defun parse-input (s)
  (let ((nums (split "," (remove #\Newline s))))
    (loop for i from 0 to (- (length nums) 1) collect (setf (gethash (parse-integer (nth i nums)) memory) i))))

;;; Exit case is 1 less than the given, because this solution starts on 0; the question starts on 1
;;; If the table has no key of the number, then the next number is 0. Otherwise, it's current iteration - value at number in table
(defun get-next (num it)
  (cond ((= it 2019) num)
        ((null (gethash num memory)) (setf (gethash num memory) it) (get-next 0 (+ it 1)))
        (t (let ((next (- it (gethash num memory))))
             (setf (gethash num memory) it) (get-next next (+ it 1))))))

(defun start ()
  (setq memory (make-hash-table))                     ; Initialise hash table
  (parse-input (get-file-string "day15/day15.csv"))   ; Parse input to populate hash table
  (get-next 0 (hash-table-count memory)))             ; Assume last number hasn't been seen before.
                                                      ; Next number is 0, iteration is length of hash table
