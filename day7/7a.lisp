(defun parse-input (l)
  (loop for b in (mapcar (lambda (s) (split "contain" (remove #\. (remove #\  s)))) l)
    collect (cons (read-from-string (unpluralise (CAR b))) (list (pair-int (split "," (CADR b)))))))

(defun pair-int (l)
  (loop for s in l if (NOT (string= s "nootherbags"))
    collect (cons (read-from-string (unpluralise (remove (digit-char (parse-integer s :junk-allowed t)) s))) (parse-integer s :junk-allowed t))))

(defun unpluralise (s)
  (string-right-trim "s" s))

(defun find-all-ancestors (target l)
  (loop for bag in l if (find target (mapcar 'car (CADR bag))) nconc (cons (CAR bag) (find-all-ancestors (CAR bag) l))))

(defun start ()
  (length (remove-duplicates (find-all-ancestors 'shinygoldbag (parse-input (get-file "day7/day7.csv"))))))
