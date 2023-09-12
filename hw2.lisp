;;;; John Bean
;;;; CISC481 Assignment 2
;;;;
;;;; Question 1
;;;; Params: a, b, c -- all integers
;;;; Returns: The middle valued integer
(defun middle_elt (a b c)
  (cond
    ((or
        (and (<= a b) (<= b c))
        (and (<= c b) (<= b a))
        ) b)
    ((or
        (and (<= b a) (<= a c))
        (and (<= c a) (<= a b))
        ) a)
    (t c)))
;;;; Question 2
;;;; Params: Integers k and l s.t. k<l, and the list 'lst'
;;;; Returns: The count of integers in 'lst' that are between 'k' and 'l' in value
(defun count_middle (k l lst)
  (cond
    ((null lst) 0)
    (
      (and (< (car lst) l) (> (car lst) k)) 
      (+ 1 (count_middle k l (cdr lst)))
    )
    (
      t (count_middle k l (cdr lst))
    )
  )
)
;;;; Question 3
;;;; Params: Integers k and l s.t. k<l, and the list 'lst'
;;;; Returns: Returns a list of elements of 'lst' that are between 'k' and 'l'
(defun collect_middle (k l lst)
  (cond
    ((null lst) nil)
    (
      (and (< (car lst) l) (> (car lst) k))
      (cons (car lst) (collect_middle k l (cdr lst))) 
    )
    (t (collect_middle k l (cdr lst)))
  )
)
;;;; Question 4
;;;; Params: A list of integers in order with the exception of one element 'lst'
;;;; Returns: The integer that is out of order in the list
;;;; Ex: '(8 14 16 2 55) -> 2
(defun find_elt (lst)
  (cond
    ((< (cadr lst) (car lst)) (cadr lst))
    (t (find_elt(cdr lst)))
  )
)
;;;; Question 5
;;;; Params: A list of integers 'lst' and an integer 'k'
;;;; Returns: A list 'lst' with all instances of 'k' removed from the list
(defun remove_elt (k lst)
  (cond
    ((null lst) nil)
    (
      (not (= k (car lst)))
      (cons (car lst) (remove_elt k (cdr lst)))
    ) 
    (t (remove_elt k (cdr lst)))
  )
)
;;;; Question 6
;;;; Params: A sorted list of integers 'lst' and an integer 'k'
;;;; Returns: A list 'lst' with integer 'k' correctly inserted
(defun insert_elt (k lst)
  (cond
    ((null lst) (list k))
    (
      (< k (car lst))
      (cons k lst)
    )
    (t (cons (car lst) (insert_elt k (cdr lst))))
  )
)