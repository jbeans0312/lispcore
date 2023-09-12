;;;; John Bean
;;;; CISC481 Assignment 2
;;;;
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

