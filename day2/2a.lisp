;;; Concatenate here turns the string into a list of chars, if a char matches the required, it is added to the count
(defun count-chars (ch string)
  (loop for i in (concatenate 'list string) if (eq i (char ch 0)) count i))

;;; Get the count of required char in string and checks it is between the required values
(defun check-valid (minmax ch pass)
  (setq c (count-chars ch pass))
  (AND (<= (CAR minmax) c) (>= (CADR minmax) c)))

;;; Splits a password specification into ((min max) char password)
(defun get-pass-details (pass)
  (setq p (split " " pass))
  (list (mapcar 'parse-integer (split "-" (CAR p))) (subseq (CADR p) 0 1) (CADDR p)))

;;; Splits each password spec string into useable details, then applies the check using them, counts successes
(defun start ()
  (loop for i in (mapcar 'get-pass-details (get-file "day2/day2.csv")) count (apply 'check-valid i)))

(defun get-file (name)
  (uiop:read-file-lines name))
