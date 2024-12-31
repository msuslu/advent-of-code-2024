(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")

(defun solve-puzzle (filename)
      (with-open-file (stream filename)
        ;; Part1
        (let ((sum 0))
          (ppcre:do-register-groups ((#'parse-integer val1 val2))
              ("mul\\((\\d+),(\\d+)\\)" (alexandria:read-file-into-string stream) nil :sharedp t)
            (incf sum (* val1 val2)))
          (print sum))
        ;; Part2
        (let ((sum 0)
              (current-state 'do)
              (content (alexandria:read-file-into-string stream))
              (scanner (ppcre:create-scanner '(:alternation
                                               (:register "do()")
                                               (:register "don't()")
                                               (:sequence
                                                (:register "mul") #\(
                                                (:register (:greedy-repetition 1 nil :digit-class)) #\,
                                                (:register (:greedy-repetition 1 nil :digit-class)) #\))) :multi-line-mode t)))
          (ppcre:do-register-groups (do dont mul (#'parse-integer val1 val2))
              (scanner content nil :sharedp t)
            (cond (do (setf current-state 'do))
                  (dont (setf current-state 'dont))
                  (mul (when (eq current-state 'do)
                         (incf sum (* val1 val2))))))
          (print sum))))
