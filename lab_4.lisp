(defun insert-right-to-left (value sorted-list &key key test)
  (let* ((key-fn (or key #'identity))
         (test-fn (or test #'<))
         (value-key (funcall key-fn value)))
    (cond
      ((null sorted-list) (list value))
      ((funcall test-fn value-key (funcall key-fn (car sorted-list)))
       (cons value sorted-list))
      (t
       (let ((rest (insert-right-to-left value (cdr sorted-list) :key key :test test)))
         (cons (car sorted-list) rest))))))

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

(defun add-next-reducer (&key transform)
  (let ((previous nil))  
    (lambda (result element)
      (let ((current (if transform (funcall transform element) element)))
        (if previous
            (setf result (append result (list (cons previous current))))) 
        (setf previous current))
      result)))

(defun add-last-pair (result previous transform)
  (if previous
      (let ((current (if transform (funcall transform previous) previous)))
        (append result (list (cons current nil)))) 
    result))

(defun test-add-next-reducer (test-number input expected &optional transform)
  (let ((result (reduce (add-next-reducer :transform transform)
                        input
                        :initial-value nil)))
    (setf result (add-last-pair result (if input (car (last input)) nil) transform))

    (if (equalp result expected) 
        (format t "Test ~A: PASSED~%" test-number)  
        (format t "Test ~A: FAILED~%" test-number)))) 



(test-add-next-reducer 1 '(1 2 3) '((1 . 2) (2 . 3) (3 . NIL))) 
(test-add-next-reducer 2 '(1 2 3) '((2 . 3) (3 . 4) (4 . NIL)) #'1+)  
(test-add-next-reducer 3 '() '())  
(test-add-next-reducer 4 '(42) '((42 . NIL)))  
(test-add-next-reducer 5 '(1 2 3) '((2 . 4) (4 . 6) (6 . NIL)) (lambda (x) (* 2 x))) 
(test-add-next-reducer 6 '(-1 -2 -3) '((-1 . -2) (-2 . -3) (-3 . NIL)))  

