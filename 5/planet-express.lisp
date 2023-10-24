;;;; John Bean
;;;; CISC481 Assignment 5
;;;; Planet Express

(defun test (planet-1 planet-2 weight)
  (PlanetExpress planet-1 planet-2 weight
		 'goal_test_PE? 'get_successors_PE
		 'get_estimate_cost_to_goal_PE))

(defun test_1 ()
  ; very easy one action test
  (test "crent" "durant" 5))
  
(defun test_2 ()
  ; easy one action test
  (test "phoe" "brattle" 6))

(defun test_3 ()
  ; easy one action test
  (test "bernar" "durant" 2))
  
(defun test_4 ()
  ; two action test where first successor of bangor is not the one that is best
  (test "baggor" "durant" 7))

(defun test_5 ()
  ; one action test where there are 2 means to get to septer
  ; and Rocket is better than Transporter
  (test "mars" "septer" 7))

(defun test_6 ()
  ; in this test, a better path is found to a node on the open list
  (test "phoe" "urie" 25))

(defun test_7 ()
  ; longer problem
  (test "phoe" "venus" 25))

(defun test_8 ()
  (test "roan" "sumver" 2))

(defun test_9 ()
  ; path of length 4
  ; finds a better path to node on open list several times
  (test "phoe" "delve" 1000))



;
; PlanetExpress takes six problem-dependant arguments:
;
;     planet-start - a string giving the name of the starting planet
;     planet-goal - a string giving the name of the goal planet
;     package-weight - the weight in pounds of the package to be delivered
;     goal_test? -  a predicate that returns true for goal nodes 
;                   and false for non-goal nodes.
;     get_successors -  a function to that returns a list of successors
;                       for a given planet 
;     get_estimate_cost_to_goal - a function which takes a planet and returns 
;                         an estimate of the cost of getting to a goal planet.

; PlanetExpress returns an optimal path from planet-start to planet-goal
; represented as a list of actions that are performed to get from
; planet-start to planet-goal
;
; States are represented as a string giving the name of the current planet
;
; Nodes in the search tree will be represented by symbols.
;     The symbol for a node is obtained by concatenating the string "Node-"
;     with the string giving the name of the planet and turning the resultant.  
;     string into a symbol via the intern function.
;     For example, the symbol for the node associated with planet mars
;     will be gotten via (intern(concatenate 'string "Node-" "mars"))
;     and will appear as |Node-mars| if printed.
; Nodes have the following properties:
;  parent - The node that is the predecessor of this node on a minimal 
;           cost path from planet-start to the state represented by the node.
;  action - The action, such as ("mars" "venus" <method>) that was used to get
;           from the previous planet to the planet represented by the node,
;           where <method> is either TRANSPORTER, LASER, or ROCKET
;  arc-cost - the cost of the action that was used to get from the previous
;             planet to the planet represented by the node
;  num-stops - the number of stops on this path
; package-weight - the weight of the package being transported
;  cost-best-path-to-state - The cost of the best known path from planet-start 
;           to the planet represented by the node.
;  cost-estimate-state-to-goal - The estimate of the cost to a goal from node
;  cost-estimate-start-to-goal - The overall estimate of the cost from 
;           planet-start to planet-goal through node
;  state - The string giving the name of the planet in the search space
;          For example, the node |Node-mars| will have "mars" as its
;          state property 

 
(defun PlanetExpress
  (planet-start planet-goal package-weight goal_test? get_successors get_estimate_cost_to_goal) 
;create a node for planet-start, and find the lowest cost path to planet-goal 
;   using Algorithm A*"
  (defun search_graph (open-closed)
  ; the value of open-closed is a symbol with property open and property
  ;   closed which give the open list and closed list respectively
  ; search_graph is the function that iterates through the open list
  ; it first selects the front node on open list and tests whether it
  ;     is the goal node.  If so, it gets the best path that has been
  ;     found to this node, along with the path cost and returns this.
  ;     Otherwise it recursively calls search_graph with the new open
  ;     and closed lists that result from expanding the graph with
  ;     the successors of the selected node.
  ; returns a 2-element list, containing the sequence of actions
					;     leading to the goal and the total cost of the path
   ; (break "entering search_graph")
     (cond((null (get open-closed 'open-list)) nil)
          (t (let((selected-node (car (get open-closed 'open-list))))
                 (terpri)
         
                 (format t "The nodes, f-values, and actions on open list are ~A" 
                            (mapcar #'(lambda (x)
                                      (list x (get x 'cost-estimate-start-to-goal) (get x 'action)))
                                    (get open-closed 'open-list)))
                 (terpri)
                 (format t "The nodes, f-values, and actions  on closed list are ~A" 
                            (mapcar #'(lambda (x)
                                        (list x (get x 'cost-estimate-start-to-goal) (get x 'action)))
                                    (get open-closed 'closed-list)))
                 (terpri) (terpri)
                 (format t "Select a new node from open list")
                 (terpri) 
                (format t "The selected node is ~A" (car (get open-closed 'open-list)))
                (terpri)
                (format t "Check if this node is the goal node")
                (terpri)
                (cond((funcall goal_test? selected-node planet-goal)
                          (get_path_and_total_cost selected-node))
                     (t (let ((successors (funcall get_successors
                                                   selected-node)))
                         (format t "Its successors are ~A"
                                  successors)
                        (terpri)
                        (setf (get open-closed 'open-list)
			      (cdr (get open-closed 'open-list)))
			(setf (get open-closed 'closed-list)
			      (cons selected-node (get open-closed 'closed-list)))
                        (search_graph
                            (expand_graph successors
                                          selected-node
                                          open-closed
                                          get_successors
                                          get_estimate_cost_to_goal
                                          planet-goal)))))))))
                         
; create a node for start-city and begin the search
 (setf (get 'X 'open-list) 
       (list (create_node planet-start package-weight 0 nil 0 nil get_estimate_cost_to_goal planet-goal)))
 (setf (get 'X 'closed-list) nil)
 (search_graph 'X)
  )
  
(defun expand_graph
   (succs parent-node open-closed succ-fn est-goal goal-planet)
;(break "entering expand_graph")
        ;; succs is the list of sucessors of parent-node 
        ;; each element of succs is a 4-tuple of the form 
        ;;    (arc-cost current-planet next-planet means) 
	;; expand_graph adds the list of successors of parent-node to 
        ;;    the graph and open list.
	;; It must make sure that a successor node has not already been
        ;;    encountered (ie., is already on open or closed) and must
        ;;    check for updating the shortest path if it has been encountered
        ;;    before
        ;; returns open-closed whose properties 'open-list and 'closed-list
        ;;    have been updated
(cond ((null succs) open-closed)
	 (t (let* ((state (caddar succs))
                   (node-name (intern (concatenate 'string "Node-" state)))
		   (arccost (caar succs))
                   (action (cdar succs))
		   (cost (+ (get parent-node 'cost-best-path-to-state)
			    arccost)))
              (format t "     The next successor is ~A" (car succs))
              (terpri)
	     ; (break "in expand_graph")
	      
              (cond ((and (not (state_on state (get open-closed 'closed-list)))
			  (not (state_on state (get open-closed 'open-list))))
; this successor is not on open or closed list
                       (format t "node not on open or closed list") 
                       (terpri)    
                       (expand_graph(cdr succs)
                                   parent-node
                                  (enter_node_on_open_list
                                  (create_node (caddar succs)
					       (get parent-node 'package-weight)
                                               arccost
                                               parent-node 
                                               cost 
                                               action 
                                               est-goal 
                                               goal-planet)
                                            open-closed)
                                      succ-fn
                                      est-goal
                                      goal-planet))
		    
		    ((and (state_on state (get open-closed 'open-list))
                          (< cost (get node-name 'cost-best-path-to-state)))
; this successor is already on open list and we have
					;    found a better path to it
		    ; (break "already on open list in expand_graph")
		     (terpri)
		     (terpri)
		     (format t "**********************")
		     (terpri)
                     (format t "****ALREADY ON OPEN AND HAS A BETTER PATH COST***")
                     (terpri)
		     (terpri)
                     (expand_graph (cdr succs)
                                    parent-node
                                   (update_node_on_open_list node-name
                                                             parent-node
							     arccost
                                                             succ-fn
                                                             cost
                                                             action
                                                             open-closed)
                                    succ-fn
                                    est-goal
                                    goal-planet))
		   
                     ((and (state_on state (get open-closed 'closed-list))
                           (< cost (get node-name 'cost-best-path-to-state)))
; this successor is already on closed list and we have
;    found a better path to it
                     (format t "*** ON CLOSED AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand_graph (cdr succs)
                                    parent-node
                                    (update_node_on_closed_list node-name
								parent-node
								arccost
                                                                succ-fn
                                                                cost
                                                                action
                                                                open-closed)
                                    succ-fn
                                    est-goal
                                    goal-planet))
		    (t 
; this successor is already on open or closed and the new path
;   to the node is not better than the existing path
                      (format t "on open or closed but path not better")
                      (terpri)
                      (expand_graph (cdr succs)
				    parent-node
				    open-closed 
				    succ-fn
				    est-goal
                                    goal-planet)))))))

(defun update_node_on_closed_list (n parent arccost successor-fn cost-of-short-path action open-closed)
; open-closed is a 2-element list whose first element is the open list
;    and whose second element is the closed list
; node n is on the closed list
; a new shortest path from starting point to node n has been found with
;   cost equal to cost-of-short-path whih goes through parent
; update the properties of the node and its descendants
;   on open and closed lists
; ONLY WRITE THIS FUNCTION FOR EXTRA CREDIT
)

(defun state_on (state lst)
; state is a string giving the name of the planet that is the state
; lst is an open or closed list
; return true if a node on lst has this planet as its state
; YOU MUST WRITE THIS FUNCTION
    (cond
        ((null lst) nil)
        ((equal
            state
            (get (car lst) 'state)
         ) t)
        (t (state_on state (cdr lst)))
    )
)

(defun place_node_on_open_list (n f-n open-list)
    (cond
        ((null open-list) (list n))
        (
            (<=
                f-n
                (+
                    (get (car open-list) 'arc-cost)
                    (get (car open-list) 'cost-estimate-state-to-goal)
                )
            )
            (cons n open-list)
        )
        (t (cons (car open-list) (place_node_on_open_list n f-n (cdr open-list))))
  )
)

(defun enter_node_on_open_list (n open-closed)
; n is a node
; the value of open-closed is a symbol with properties 'open-list and 'closed-list
; add n to the open list in the correct position 
; return the modified open-closed
; YOU MUST WRITE THIS FUNCTION
    (setf (get open-closed 'open-list)
      (place_node_on_open_list 
          n 
          (+
              (get n 'arc-cost) 
             (get n 'cost-estimate-state-to-goal)
          )
          (get open-closed 'open-list)
      )
    )
    open-closed
    ; ERROR, WE ARE UPDATING OPEN-LIST, BUT NOT OPEN-CLOSED
;(break "entering enter_node_on_open_list")
)

(defun remove_elt (n open-list)
    (cond
      ((null open-list) nil)
      (
        (not (equal (get n 'state) (get (car open-list) 'state)))
        (cons (car open-list) (remove_elt n (cdr open-list)))
      ) 
      (t (remove_elt n (cdr open-list)))
    )
)

(defun adjust_open_list (n open)
;(break "entering adjust_open_list")
; n is a node
; open is an open list of nodes 
; make sure that n is in its proper position on the open list, and if not
;   move it to the proper position
; the reason that n may not be in its proper position is that a better
;   path to it may have been found, thereby changing its f value
; return the revised open list
; YOU MUST WRITE THIS FUNCTION
  (place_node_on_open_list 
      n
      (+
        (get n 'arc-cost)
        (get n 'cost-estimate-state-to-goal)
      )
      (remove_elt n open)
  )
  open
)

(defun create_node 
  (planet weight arc-cost parent cost-of-short-path action est-goal goal-planet)
  ; create node with the appropriate properties
  ; return the created node.
  ;(break "in create_node")
(let ((node (intern (concatenate 'string 
                                 "Node-"
				 planet
                                 ))))
  (setf (get node 'state) planet)
  (setf (get node 'num-stops)
	(cond ((null action) 0)
	      (t (+ 1 (get parent 'num-stops)))))
  (setf (get node 'package-weight) weight)
  (setf (get node 'arc-cost) arc-cost)
  (setf (get node  'parent) parent)
  (setf (get node 'action) action)
  (setf (get  node 'cost-best-path-to-state) cost-of-short-path)
  (setf (get node 'cost-estimate-state-to-goal)
	(funcall est-goal planet goal-planet )) 
  (setf (get  node `cost-estimate-start-to-goal)
        (+ cost-of-short-path (get node 'cost-estimate-state-to-goal)))
  node)
)

(defun update_node_on_open_list 
  (n parent arccost successor-fn cost-of-short-path action open-closed )
  ; the value of open-closed is a symbol with properties 'oepn-list and 'closed-list
  ; node n is on the open list
  ; a new shortest path from the starting planet to node n has been found with
  ;  cost equal to cost-of-short-path which goes through parent.  
  ; update the properties of the node and, if necessary, its position
  ;  on open list 
  ; return the modified open-closed 
; revise_node_on_open_list should call your function adjust_open_list
; YOU MUST WRITE THIS FUNCTION

    ; use action to update action
    ; update parent with new parent
    ; use arccost to update arc-cost
    ; use cost-of-short-path + to update cost-best-path-to-state
    ; use (get parent 'num-stops +1) to update num-stops

    (setf (get n 'action) action)
    (setf (get n 'parent) parent)
    (setf (get n 'arc-cost) arccost)
    (setf (get n 'cost-best-path-to-state) cost-of-short-path)
    (setf (get n 'num-stops) (+ 1 (get parent 'num-stops)))
    (setf (get open-closed 'open-list) (adjust_open_list n (get open-closed 'open-list)))
    open-closed
)

(defun get_path (node)
  (cond
    ((null (get node 'parent)) nil)
    (t
      (cons 
        (get node 'action)
        (get_path 
          (intern 
            (concatenate 
            'string "Node-" (car (get node 'action))
            )
          )
        )
      )
    )
  )
)

(defun get_total_cost (node)
  (cond
    ((null (get node 'parent)) 0)
    (t (+ 
      (get node 'arc-cost)
      (get_total_cost
        (intern 
          (concatenate 
            'string "Node-" (car (get node 'action))
          )
        )
      )
    ))
  )
)

(defun get_path_and_total_cost (node)
; node is a node in the graph
; return a list consisting of two elements: the path (in terms of actions) 
;    that was taken to get to node and the cost of that path
; YOU MUST WRITE THIS FUNCTION

; use (get node 'action) to recursively backtrack until we have reached the start
; we will know we are at the start node if action = nil
; keep a running sum of the arc-cost
  (list (reverse (get_path node)) (get_total_cost node))
)

(defun sq_diff (pos1 pos2)
  ; pos1 and pos2 are each integers representing the x, y, or z coordinate of a planet
  ; return is the squared different between the two positions
    (let ((diff (- pos2 pos1)))
        (* diff diff)
    )
)
(defun get_distance (p1 p2)
  ; p1 and p2 are both strings representing the names of planets, such as "mars", "septer"
  ; return is the straight line distance between the two planets
  ; utilizes sq_diff to help with calculations
    (round (sqrt 
      (+ 
        (sq_diff (get (intern p2) 'x-coord) (get (intern p1) 'x-coord))
        (sq_diff (get (intern p2) 'y-coord) (get (intern p1) 'y-coord))
        (sq_diff (get (intern p2) 'z-coord) (get (intern p1) 'z-coord))
      )
    ))
)

(defun calc_arc-cost (node succ method)
; node is the node we are finding the successors of
; intern-succ is the string representing the name of the successor planet
; method is the symbol representing the mode of transportation between the two planets
; CALCULATIONS GUIDE...
; ROCKET: $500 plus $10 for each million miles between the two planets plus $75 times number of
; stops made thus far, rounded to an integer
; LASER:$1000 for each pound that the package weighs plus $1 for each million miles between the two
; planets
; TRANSPORTER: $15 for each million miles between the two planets plus $20 for each pound that
; the package weighs, rounded to an integer
  (let 
    ((distance (get_distance (get node 'state) succ)))
    (cond 
      ((equal method (intern "ROCKET"))
        (+ 500 (* 10 distance) (* 75 (get node 'num-stops)))
      )
      ((equal method (intern "LASER"))
        (+ distance (* 1000 (get node 'package-weight)))
      )
      ((equal method (intern "TRANSPORTER"))
        (+ (* 15 distance) (* 20 (get node 'package-weight)))
      )
    )
  )
)

(defun build_successors (node next-planet)
  ; node is the node we are collecting the successors of
  ; next-planet is the (get (intern (get node 'state) 'next-planet))
  ; return is the list for get_successors_PE
  (cond 
    ((null next-planet) nil)
    (t 
      (cons 
        (list 
          (calc_arc-cost 
            node 
            (car (car next-planet))
            (car (cdr (car next-planet)))
          )
          (get node 'state)
          (car (car next-planet))
          (car (cdr (car next-planet)))
        )
        (build_successors node (cdr next-planet))
      )
    )
  )
)

(defun get_successors_PE (node)
; node is a node 
; return a list of the successors of the node, with each successor given as
;   a 4-tuple of the form (arc-cost current-planet next-planet means), such as 
;   (cost "mars" "venus" ROCKET) 
; YOU MUST WRITE THIS FUNCTION
;(break "in get_successors_PE")
  (build_successors node (get (intern (get node 'state)) 'next-planet))
)

(defun goal_test_PE? (node goal-planet)
; node is a node and goal-planet is a string giving the
;    name of the goal planet
; return true if the state for this node is goal-planet
; YOU MUST WRITE THIS FUNCTION
  (equal (get node 'state) goal-planet)
)

(defun get_estimate_cost_to_goal_PE (planet goal-planet)
; planet and goal-planet are both strings giving the names of planets
; return an estimate of the cost of getting from planet to goal-planet
; For this project, the estimate will be the straight-line distance
;    between the planets  
; YOU MUST WRITE THIS FUNCTION
  ;(break "entering get_estimate_cost_to_goal_PE")
  (get_distance planet goal-planet)
)

