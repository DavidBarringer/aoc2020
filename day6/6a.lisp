;;; This splits input by double newline, then removes newlines in the list,
;;; then removes all duplicate characters
(defun parse-input-unique (l)
  (mapcar (lambda (s) (remove-duplicates (remove #\Newline (remove #\Return s)) :test 'char-equal))
          (split (format nil "~C~%~C~%" #\Return #\Return) l)))

(defun start ()
  (apply '+ (mapcar 'length (parse-input-unique (get-file-string "day6/day6.csv")))))
