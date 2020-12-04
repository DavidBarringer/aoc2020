(defun parse-input (l)
  (let ((l1 (mapcar (lambda (s) (split "," s)) (mapcar (lambda (s) (substitute #\, #\  s)) (split ",," (format nil "~{~A,~}" l))))))
          (loop for x in l1 collect (mapcar (lambda (s) (split ":" s)) x))))

;;; This function works similar to part a, but applies a regex check from the check list on the value before verifying.
(defun check-valid-r (l check)
    (= 7 (loop for f1 in check count (loop for f2 in l if (string= (CAR f2) (CAR f1))
                                                        if (apply 'scan-to-strings (list (CDR f1) (CADR f2))) collect t))))

;;; Same as part a but with a regex paired to each field
(defun start-r()
  (let ((check '(("ecl" . "amb|blu|brn|gry|grn|hzl|oth")
                 ("pid" . "^[0-9]{9}$")
                 ("eyr" . "20(2[0-9])|2030")
                 ("hcl" . "#[0-9a-f]{6}")
                 ("byr" . "(19[2-9][0-9])|(200[0-2])")
                 ("iyr" . "20(1[0-9])|2020")
                 ("hgt" . "((59|(6[0-9])|7[0-6])in)|(((1[5-8][0-9])|19[0-3])cm)")))) ; A pair of each list and their check regex
          (loop for pp in (parse-input (get-file "day4/day4.csv")) if (check-valid-r pp check) count pp)))
