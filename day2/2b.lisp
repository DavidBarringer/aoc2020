;;; Lisp doesn't have an XOR macro, I'm to lazy to make one.
;;; This checks each position for the required character then XORs the result.
(defun char-pos (ch string p1 p2)
  (let ((b1 (eq ch (nth (- p1 1) string)))
        (b2 (eq ch (nth (- p2 1) string))))
          (NOT (eq b1 b2))))

;;; Changes the input into a useable form for char-pos
(defun check-valid (pos ch pass)
  (char-pos (char ch 0) (concatenate 'list pass) (CAR pos) (CADR pos)))

(defun get-pass-details (pass)
  (let ((p (split " " pass)))
    (list (mapcar 'parse-integer (split "-" (CAR p))) (subseq (CADR p) 0 1) (CADDR p))))

;;; Same as day 1
(defun start ()
  (loop for i in (mapcar 'get-pass-details (get-file "day2/day2.csv")) count (apply 'check-valid i)))
