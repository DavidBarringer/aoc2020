;;; Start by stipping "." and " " from the input, then splits on "contain".
;;; Then go through that list and makes a list with elements that look like: (colour ((colour . num) ...))
(defun parse-input (l)
  (loop for b in (mapcar (lambda (s) (split "contain" (remove #\. (remove #\  s)))) l)
    collect (cons (read-from-string (unpluralise (CAR b))) (list (pair-int (split "," (CADR b)))))))

;;; Takes the list of bags contained, makes a dotted pair of colour and number for each and returns a list of them
(defun pair-int (l)
  (loop for s in l if (NOT (string= s "nootherbags"))
    collect (cons (read-from-string (unpluralise (remove (digit-char (parse-integer s :junk-allowed t)) s)))
                  (parse-integer s :junk-allowed t))))

;;; Removes "bag(s)" from the end of a string
(defun unpluralise (s)
  (string-right-trim "bags" s))

;;; Checks the contains list of each bag, if the target bag is there, the bag gets added to the list and gets searched for
(defun find-containers (target l)
  (loop for bag in l if (find target (mapcar 'car (CADR bag))) nconc (cons (CAR bag) (find-containers (CAR bag) l))))

;;; Finds each bag containing target and each bag containing that etc. Removes repeats and gets the length of the final list
(defun start ()
  (length (remove-duplicates (find-containers 'shinygold (parse-input (get-file "day7/day7.csv"))))))
