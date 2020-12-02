;;; Lisp doesn't have an XOR macro, I'm to lazy to make one.
;;; This checks each position for the required character then XORs the result.
(defun char-pos (ch string p1 p2)
  (AND (OR (eq ch (nth (- p1 1) string)) (eq ch (nth (- p2 1) string)))
       (NOT (AND (eq ch (nth (- p1 1) string)) (eq ch (nth (- p2 1) string))))))

;;; Changes the input into a useable form for char-pos
(defun check-valid (pos ch pass)
  (char-pos (char ch 0) (concatenate 'list pass) (CAR pos) (CADR pos)))

(defun get-pass-details (pass)
  (setq p (split " " pass))
  (list (mapcar 'parse-integer (split "-" (CAR p))) (subseq (CADR p) 0 1) (CADDR p)))

;;; Same as day 1
(defun start ()
  (loop for i in (mapcar 'get-pass-details (get-file "day2/day2.csv")) count (apply 'check-valid i)))

(defun get-file (name)
  (uiop:read-file-lines name))
