;;; This splits on double newline then splits strings in resulting list by single newline
(defun parse-input (l)
  (mapcar (lambda (s) (split #\Newline s)) (split (format nil "~%~%") l)))

;;; Different to part a - This runs parse-input, then concatenates the strings in each list.
;;; These strings also have duplicates removed and get split into a list of characters
(defun parse-input-unique (l)
  (loop for s in (parse-input l) collect (concatenate 'list (remove-duplicates (apply #'concatenate 'string s) :test 'char-equal))))

;;; Goes through each group tries to find each unique character in each answer
;;; If any answer doesn't contain the charcater, the character is not counted
(defun parse-input-check (l)
  (loop for group in (parse-input l) for target in (parse-input-unique l) sum
    (loop for c in target if (notany 'null (mapcar (lambda (s) (find c s)) group)) count c)))

(defun start ()
  (parse-input-check (get-file-string "day6/day6.csv")))
