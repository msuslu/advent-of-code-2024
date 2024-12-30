
(defun parse-str (str)
  "Parse intergers splitted by spaces in a line and return a list of them."
  (let ((pos 0)
        (end (length str))
        (result nil))
     (loop while (< pos end)
           do (multiple-value-bind (num p)
                    (parse-integer str :start pos :junk-allowed t)
                (setf pos p)
                (push num result)
                )
           finally (return (nreverse result)))))


(defun load-data (filename)
  "Load data from the file and return a list of integer list."
  (with-open-file (stream filename)
      (loop for line = (read-line stream nil nil)
            while line
            collect (parse-str line))))

(defun remove-nth (n lst)
  "Remove the nth element from the list."
  (loop for i from 0 to (1- (length lst))
        unless (= i n)
        collect (nth i lst)))

(defun solve-puzzle (data)
  ;; Part 1
    (loop with safe = 0
          for l in data
          do (let ((direction nil)
                   (end (- (length l) 2)))
               (loop for i from 0 to end
                     do (let ((diff (- (nth i l) (nth (+ i 1) l))))
                          (if (and  (not  (= (abs diff) 0)) (<= (abs diff) 3))
                              (cond ((and (eql direction 'up) (< diff 0)) (return))
                                    ((and (eql direction 'down) (> diff 0)) (return))
                                    ((and (eql direction nil) (> diff 0)) (setf direction 'up))
                                    ((and (eql direction nil) (< diff 0)) (setf direction 'down)))
                              (return)))
                     finally (if (= i (+ end 1))
                                 (incf safe))))
             finally (print safe))

  ;; Part 2
    (loop with safe = 0
          for l in data
          do (block outer
                 (loop for i from -1 to (- (length l) 1)
                       do (let* ((direction nil)
                                 (sl (remove-nth i l))
                                 (end (- (length sl) 2)))
                            (loop for j from 0 to end
                                  do (let ((diff (- (nth j sl) (nth (+ j 1) sl))))
                                       (if (and  (not  (= diff 0)) (<= (abs diff) 3))
                                           (cond ((and (eql direction 'up) (< diff 0)) (return))
                                                 ((and (eql direction 'down) (> diff 0)) (return))
                                                 ((and (eql direction nil) (> diff 0)) (setf direction 'up))
                                                 ((and (eql direction nil) (< diff 0)) (setf direction 'down)))
                                           (return)))
                                  finally (if (= j (+ end 1))
                                              (return-from outer (incf safe)))))))
          finally (print safe)))
