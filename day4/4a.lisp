;;; For some reason, I couldn't split on double newline. This is the workaround.
;;; Takes in list, combines each element into a string (effectively ridding new lines) seperated by ","
;;; Since 2x newline has ",," now, I can split by that. Then, for consistency, I replace all spaces with ",".
;;; This allows me to split by "," for a list of lists of (field:value), finally I split that list by ":"
(defun parse-input (l)
  (let ((l1 (mapcar (lambda (s) (split "," s)) (mapcar (lambda (s) (substitute #\, #\  s)) (split ",," (format nil "~{~A,~}" l))))))
          (loop for x in l1 collect (mapcar (lambda (s) (split ":" s)) x))))

;;; Checks that each required field name is in a passport, then counts each pass.
;;; If number of passes = 7 then return true
(defun check-valid (pp check)
  (= 7 (loop for field in check if (member field (mapcar 'CAR pp) :test 'string=) count field)))

;;; Goes through each passport and counts those that have the 7 needed fields
(defun start()
  (let ((check '("ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"))) ; A list of each field that is needed
          (loop for pp in (parse-input (get-file "day4/day4.csv")) if (check-valid pp check) count pp)))
