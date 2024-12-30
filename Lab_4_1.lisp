
(defun insert-right-to-left (value sorted-list &key key test)
  (let* ((key-fn (or key #'identity))
         (test-fn (or test #'<))
         (value-key (funcall key-fn value)))
    (loop for element in sorted-list
          with result = '()
          until (funcall test-fn value-key (funcall key-fn element))
          do (push element result)
          finally (return (nconc (reverse result) (list value) (remove-if (lambda (x) (member x (reverse result))) sorted-list))))))


(defun insertion-sort-functional (unsorted-list &key key test)
 (let ((key-fn (or key #'identity))
        (test-fn (or test #'<)))
    (reduce (lambda (sorted element)
              (insert-right-to-left element sorted :key key-fn :test test-fn))
            unsorted-list
            :initial-value nil)))

;; Tests for the functional version
(format t "Functional Sort: ~A -> ~A~%" '(3 1 4 1 5 9) 
        (insertion-sort-functional '(3 1 4 1 5 9) :key #'identity :test #'<))



(defun insertion-sort-imperative (list &key key test)
  (let* ((sorted-list (copy-list list))
         (key-fn (or key #'identity))
         (test-fn (or test #'<)))
    (loop for i from 1 below (length sorted-list) do
      (let ((key-value (funcall key-fn (nth i sorted-list)))
            (j (- i 1)))
        (loop while (and (>= j 0)
                         (funcall test-fn key-value (funcall key-fn (nth j sorted-list))))
              do
              (setf (nth (+ j 1) sorted-list) (nth j sorted-list))
              (decf j))
        (setf (nth (+ j 1) sorted-list) (nth i list))))
    sorted-list))

;; Tests for the imperative version
(format t "Imperative Sort: ~A -> ~A~%" '(3 1 4 1 5 9) 
        (insertion-sort-imperative '(3 1 4 1 5 9) :key #'identity :test #'<))



;; add-next-reducer


(defun add-next-reducer (&key (transform #'identity))
  (lambda (result element)
    (let ((current (funcall transform element)))
      (if (null result)
          (list (cons current nil))
          (let* ((last-pair (car (last result)))
                 (updated-last (cons (car last-pair) current))
                 (new-pair (cons current nil)))
            (append (butlast result)
                    (list updated-last new-pair)))))))






(defun test-add-next-reducer (test-number input expected &optional transform)
  (let ((result (reduce (add-next-reducer :transform (or transform #'identity))
                        input
                        :initial-value nil)))
    (if (equalp result expected)
        (format t "Test ~A: PASSED~%" test-number)
        (format t "Test ~A: FAILED (expected ~A, got ~A)~%" test-number expected result))))




(test-add-next-reducer 1 '(1 2 3) '((1 . 2) (2 . 3) (3 . NIL))) 
(test-add-next-reducer 2 '(1 2 3) '((2 . 3) (3 . 4) (4 . NIL)) #'1+)  
(test-add-next-reducer 3 '() '())  
(test-add-next-reducer 4 '(42) '((42 . NIL)))  
(test-add-next-reducer 5 '(1 2 3) '((2 . 4) (4 . 6) (6 . NIL)) (lambda (x) (* 2 x))) 
(test-add-next-reducer 6 '(-1 -2 -3) '((-1 . -2) (-2 . -3) (-3 . NIL)))  

