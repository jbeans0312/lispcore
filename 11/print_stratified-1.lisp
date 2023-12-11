(defun print_stratified (stratified-list)
; stratified-list is the list returned by get_stratified_set			; prints the instances in each class	     
      (terpri)
      (print "The instances in Class 1 are")
      (terpri)
      (print (get_instances_in_class stratified-list 1))
      (terpri)
      (print "The instances in Class 2 are")
      (terpri)
      (print (get_instances_in_class stratified-list 2))
      (terpri)
      (print "The instances in Class 3 are")
      (terpri)
      (print (get_instances_in_class stratified-list 3))
      (terpri)
      (print "The instances in Class 4 are")
      (terpri)
      (print (get_instances_in_class stratified-list 4))
      (terpri)
      (print "The instances in Class 5 are")
      (terpri)
      (print (get_instances_in_class stratified-list 5))
      (terpri)
      (print "The instances in Class 6 are")
      (terpri)
      (print (get_instances_in_class stratified-list 6))
      (terpri)
      (print "The instances in Class 7 are")
      (terpri)
      (print (get_instances_in_class stratified-list 7))
      (terpri)
      )

 (defun get_instances_in_class (lst class)
  (cond ((null lst) nil)
	((equal (get (car lst) 'TYPE?) class)
		(cons (car lst) (get_instances_in_class (cdr lst) class)))
	(t (get_instances_in_class (cdr lst) class))))
