<p align="center"><b>Національний технічний університет України “Київський політехнічний інститут ім. Ігоря Сікорського”</b></p>
<p align="center"><b>Факультет прикладної математики Кафедра системного програмування і спеціалізованих комп’ютерних систем</b></p>
<p align="center"><b>ЛАБОРАТОРНА РОБОТА №4</b></p>
<p align="center"><b>з дисципліни «Вступ до функціонального програмування»</b></p>

<div align="right">
    <p>Студент: Горбик Данііл</p>
    <p>Група: КВ-13</p>
    <p>Рік: 2024</p>
</div>
## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно. 
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
	- використати функції вищого порядку для роботи з послідовностями (де це доречно);
	- додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: key та test , що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому key має виконатись мінімальну кількість разів
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.

## Завдання за варіантом №4

1. Алгоритм сортування вибором за незменшенням.

```lisp
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

```

2. Написати функцію add-next-reducer , яка має один ключовий параметр — функцію transform . add-next-reducer має повернути функцію, яка при застосуванні в якості першого аргументу reduce робить наступне: кожен елемент списку-аргументу reduce перетворюється на точкову пару, де в комірці CAR знаходиться значення поточного елемента, а в комірці CDR знаходиться значення наступного елемента списку (тобто того, що знаходиться "справа"). Якщо функція transform передана, тоді значення поточного і наступного елементів, що потраплять у результат, мають бути змінені згідно transform . Обмеження, які накладаються на використання функції-результату addnext-reducer при передачі у reduce визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції reduce from-end та initial-value ). transform має виконатись мінімальну кількість разів.

```lisp

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



```

3. Тести

```lisp

(defun test-add-next-reducer (test-number input expected &optional transform)
  (let ((result (reduce (add-next-reducer :transform transform)
                        input
                        :initial-value nil)))
    (setf result (add-last-pair result (if input (car (last input)) nil) transform))

    (if (equalp result expected)
        (format t "Test ~A: PASSED~%" test-number)
        (format t "Test ~A: FAILED~%" test-number))))

;; Tests for the functional version
(format t "Functional Sort: ~A -> ~A~%" '(3 1 4 1 5 9)
        (insertion-sort-functional '(3 1 4 1 5 9) :key #'identity :test #'<))


;; Tests for the imperative version
(format t "Imperative Sort: ~A -> ~A~%" '(3 1 4 1 5 9)
        (insertion-sort-imperative '(3 1 4 1 5 9) :key #'identity :test #'<))



(test-add-next-reducer 1 '(1 2 3) '((1 . 2) (2 . 3) (3 . NIL)))
(test-add-next-reducer 2 '(1 2 3) '((2 . 3) (3 . 4) (4 . NIL)) #'1+)
(test-add-next-reducer 3 '() '())
(test-add-next-reducer 4 '(42) '((42 . NIL)))
(test-add-next-reducer 5 '(1 2 3) '((2 . 4) (4 . 6) (6 . NIL)) (lambda (x) (* 2 x)))
(test-add-next-reducer 6 '(-1 -2 -3) '((-1 . -2) (-2 . -3) (-3 . NIL)))

```

## Результат виконання програми

```
Functional Sort: (3 1 4 1 5 9) -> (1 1 3 4 5 9)
Imperative Sort: (3 1 4 1 5 9) -> (1 1 3 4 5 9)
Test 1: PASSED
Test 2: PASSED
Test 3: PASSED
Test 4: PASSED
Test 5: PASSED
Test 6: PASSED
```
