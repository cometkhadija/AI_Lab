(defun read-list (prompt)
  "Read a list of numbers from user"
  (format t "~a " prompt)
  (force-output)                  
  (let ((input (read)))
    (if (listp input)
        input
        (progn
          (format t "~%Error: Please enter a proper list like (1 2 3)~%")
          (read-list prompt)))))     

(defun list-union (list1 list2)
  "Return the union of two lists (numbers only)"
  (remove-duplicates (append list1 list2) :test #'=))

(defun list-intersection (list1 list2)
  "Return the intersection of two lists (numbers only)"
  (remove-if-not (lambda (x) (member x list2 :test #'=)) list1))

(defun list-max-min (lst)
  "Return max and min of a list"
  (if (null lst)
      (values nil nil)              ; empty list 
      (values (apply #'max lst) (apply #'min lst))))

(defun numbers-to-comma-string (numbers)
  "Convert list of numbers to comma separated string (no trailing comma)"
  (if (null numbers)
      "empty"
      (format nil "~{~a~^, ~}" numbers)))

;; -------------------------------
;; Main Program
;; -------------------------------
(let* ((list1 (read-list "Enter first list: "))
       (list2 (read-list "Enter second list: "))
       (union-list    (list-union list1 list2))
       (inter-list    (list-intersection list1 list2))
       (combined-list (append list1 list2)))

  (format t "~%Union between two lists: ~a~%"
          (numbers-to-comma-string union-list))
  
  (format t "Intersection between two lists: ~a~%"
          (numbers-to-comma-string inter-list))

  
  (terpri)   ; extra newline for better look
  
  (multiple-value-bind (max1 min1) (list-max-min list1)
    (format t "First list → Max: ~a    Min: ~a~%" 
            (or max1 "N/A") (or min1 "N/A")))

  (multiple-value-bind (max2 min2) (list-max-min list2)
    (format t "Second list → Max: ~a    Min: ~a~%" 
            (or max2 "N/A") (or min2 "N/A")))

  (multiple-value-bind (maxc minc) (list-max-min combined-list)
    (format t "Combined lists → Max: ~a    Min: ~a~%" 
            (or maxc "N/A") (or minc "N/A"))))
