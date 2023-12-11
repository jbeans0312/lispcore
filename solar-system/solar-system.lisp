;;;; John Bean
;;;; CISC481 Assignment 3
;;;;
;;;; Question 1.1
;;;; Params: Two lists 'lst1' 'lst2'
;;;; Returns: The intersections of 'lst1' 'lst2' with elements in original order
;;;; Utilizes check_membership to check if element is in a list
(defun check_membership (k lst)
    (cond
        ((null lst) nil)
        ((equal k (car lst)) t)
        (t (check_membership k (cdr lst)))
    )
)
(defun intersect (lst1 lst2)
    (cond
        ((null lst1) nil)
        ((check_membership (car lst1) lst2)
            (cons (car lst1) (intersect (cdr lst1) lst2))
        )
        (t (intersect (cdr lst1) lst2))
    )
)
;;;; Question 1.2
;;;; Params: A list 'lst'
;;;; Returns: A list containing all non-duplicate members of 'lst'
;;;; Utilizes check_membership to check if an element has a duplicate in the list
(defun remove_duplicates (lst)
    (cond
        ((null lst) nil)
        ((not (check_membership (car lst) (cdr lst)))
            (cons (car lst) (remove_duplicates (cdr lst)))
        )
        (t (remove_duplicates (cdr lst)))
    )
)
;;;; Question 2.1
;;;; Params: Two strings representing two planets in our solar system
;;;; Returns: The distance between them in 3D space
;;;; Utilizes sq_diff to assist with calculations
(defun sq_diff (pos1 pos2)
    (let ((diff (- pos2 pos1)))
        (* diff diff)
    )
)
(defun get_distance (p1 p2)
    (isqrt 
        (+ 
            (sq_diff (get (intern p2) 'x-coord) (get (intern p1) 'x-coord))
            (sq_diff (get (intern p2) 'y-coord) (get (intern p1) 'y-coord))
            (sq_diff (get (intern p2) 'z-coord) (get (intern p1) 'z-coord))
        )
    )
)
;;;; Question 2.2
;;;; Params: Two strings: 'pl' represents the planet and 'tr' represents the mode of transportation
;;;; Returns: A list of all planets one could get to from planet 'pl' using transport 'tr'
(defun check_transport (sys tr)
    (cond
        ((null sys) nil)
        ((equal (car (cdr (car sys))) tr)
            (cons (car (car sys)) (check_transport (cdr sys) tr))
        )
        (t (check_transport (cdr sys) tr))
    )
)
(defun get_planets_by_means (pl tr)
    (check_transport (get (intern pl) 'next-planet) tr)
)
;;;; Question 2.3
;;;; Params: A string representing the name of a planet
;;;; Returns: A list of 4-tuples in the form of ((cost, start, end, transport))
(defun get_actions (sys pl)
    (cond
        ((null sys) nil)
        (t 
            (cons 
                (list
                    (get_distance pl (car (car sys)))
                    pl
                    (car (car sys))
                    (car (cdr (car sys)))
                )
                (get_actions (cdr sys) pl)
            )
        )
    )
)
(defun construct_action_list (pl)
    (get_actions (get (intern pl) 'next-planet) pl)
)
