(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")

(defun solve-puzzle (filename)
      (with-open-file (stream filename)
        (let ((sum 0))
          ;; Part1
          (ppcre:do-register-groups ((#'parse-integer val1 val2)) ("mul\\((\\d+),(\\d+)\\)" (alexandria:read-file-into-string stream) nil :sharedp t)
            (incf sum (* val1 val2)))
          (print sum))
        ;; Part2
        (let ((sum 0)
              (current-state 'do)
              (content (alexandria:read-file-into-string stream)))
          (ppcre:do-register-groups (state (#'parse-integer val1 val2)) ("(?:(do|don't)\\(\\))?.*?mul\\((\\d+),(\\d+)\\)" content nil :sharedp t)
            (format t "~%~a ~a ~a" state val1 val2)
            (when state
              (cond ((string= state "do") (setf current-state 'do))
                    ((string= state "don't") (setf current-state 'dont))))
            (when (eq current-state 'do)
              (incf sum (* val1 val2))))
          (print sum))))
