(defun parse-line (line)
  "Parse two integers from a line of text."
  (multiple-value-bind (num1 pos)
      (parse-integer line :junk-allowed t)
    (list num1 (parse-integer line :start pos :junk-allowed t))))

(defun load-data (filename)
  "Load the data from the file and parse it into a list."
  (with-open-file (stream filename)
      (loop for line = (read-line stream nil nil)
            while line
            collect (parse-line line))))

(defun solve_p1 (data)
  "Sort the source and the destination lists and sum the absolute difference between the two."
  (let ((src (sort (mapcar #'first data) #'<))
        (dest (sort (mapcar #'second data) #'<)))
    (loop for s in src
          for d in dest
          sum (abs (- s d)))))

(defun solve_p2(data)
  "Find the frequencies in the destination list and sum the product of the source and frequency."
  (let ((src (mapcar #'first data))
        (dst (mapcar #'second data))
        (count (make-hash-table))
        (sum 0))
    (loop for d in dst
          do (incf (gethash d count 0)))
    (loop for s in src
          do (setf sum (+ sum (* s (gethash s count 0))))
             finally (return sum))))
