(defun testfun (x)
  (* x x))

(defparameter test-1 '(1 2 3)) ; Test set 1
(defparameter test-2 '(2 4 6)) ; Test set 2


;            1
;
;; Return T if item is a member of set.
;; Return NIL if item is not a member of set.
;; The type of set is list.
;; Examples:
;;  (set-member '(1 2) 1) => T
;;  (set-member '(1 2) 3) => NIL
(defun set-member (set item)
  (cond 
    ((null set) nil)  ; Base case that sees if a set is empty  then it returns nil
    ((equal item (car set)) t)  ; Return T if first atom is item
    (t (set-member (cdr set) item))) ; Calls this function recursively 
  )


;            2
;
;; Return the union of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-union '(1 2) '(2 4)) => '(1 2 4)
(defun set-union (set-1 set-2)
  (if (null set-1) set-2 
       ; Base case if set-1 is empty return set-2
      (if (set-member set-2 (car set-1))
	; Checks if first atom is already in list-2
	(set-union (cdr set-1) set-2)
	; If it is, the fist atom in set-1 wont get looked at
	(set-union (cdr set-1) (cons (car set-1) set-2))))
	 ; Adds atom to new set if it isnt

  )


;            3
;
;; Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)
(defun set-intersection (set-1 set-2) 
  (if (equal set-1 '()) ;check if set-1 is empty (means empty intersection).
      '()
      (if (set-member set-2 (car set-1))
       ;checks if the first element of set-1 is also a member of set-2
          (cons (car set-1) (set-intersection (cdr set-1) set-2))
	  ;first item in set-1 is a member of set-2, creates a new list,
	  ; checks rest of list.
          (set-intersection (cdr set-1) set-2))))
	   ;first item fo set-1 is not a member of set-2, check rest of list




;             4
;
;; Return the difference of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-diff '(1 2) '(2 4)) => '(1)
(defun set-diff (set-1 set-2)
   (if (equal set-1 '())
      '()
      (if (set-member set-2 (car set-1)) ;same as set-intersection
          (set-diff (cdr set-1) set-2)
        ;first element is a member of set-2, recursively call set-diff
        ;to check rest of list.
          (cons (car set-1) (set-diff (cdr set-1) set-2)))))
      	;first item(set-1) not a member of set-2 then make a list and
	   ;continue to check through recursion.



;             5
;
;; Return the exclusive or of a and b
;; Examples:
;;  (boolean-xor t nil) => t
;;  (boolean-xor nil nil) => nil
(defun boolean-xor (a b)
 (and (or a b) (not (and a b)))
) 



;             6
;
;; Return the implication of a and b
;; Examples:
;;  (boolean-implies t nil) => nil
;;  (boolean-implies nil nil) => t
(defun boolean-implies (a b)
    (cond 
    ((and (equal a t) (equal b t)) t) ; If a & b = true, return true
    ((and (equal a nil) (equal b nil)) t) ; If a & b = false, return true
    ((and (equal a nil) (equal b t)) t) ; If a is nil and b is t, return true
    (t nil)) ; The other cases all return nil
)



;             7
;
;; Return the bi-implication (if and only if) of a and b
;; Examples:
;;  (boolean-iff t nil) => nil
;;  (boolean-iff nil nil) => t

(defun boolean-iff (a b)
 (and (or (not a) b) (or (not b) a))
)




;             8
;
;; Evaluate a boolean expression.
;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
;; Examples:
;;  (boolean-eval '(and t nil)) => nil
;;  (boolean-eval '(and t (or nil t))) => t


(defun boolean-eval (exp)
   (format t "~%~%> Expression:  ~a" exp)
  (let* (
	 (bool_operator (car exp)) 
	 (operands (cdr exp)) 
	 (operands_len (length operands))
	 (left_operand (car operands)) 
	 )
   
    (format t "~%~%~%bool_operator :~T~T~T~T~T  ~a" bool_operator)
    (format t "~%operands:~T~T~T~T~T~T~T~T~T  ~a" operands)
    (format t "~%operand list length:~T~T ~a" operands_len)
    (format t "~%left_operand:~T~T~T~T~T~T~T  ~a" left_operand)
    (format t "~%right operand:~T~T~T~T~T~T~T ~a" (car(cdr operands)))
    
   
  (format t "~%~%____________________________________________" )

;______________________Main cond starts here_____________
    (cond
      (   ; length == 1
	   (equal operands_len 1)			
	  (if (atom left_operand) (not left_operand) ( not(boolean-eval left_operand)))
	   
       )
	

      (	; length == 2

       (equal operands_len 2) ; condition to start sub cond
;______________________Sub cond starts here

       (cond
;--------------------------------------------------------------------------------
;---and------ "and" expression check
         (  
	   (equal bool_operator 'and) ; bool_operator == and
				
	   (if (atom (car(cdr operands)))
	       ; if true
	        (and left_operand  (car(cdr operands)))
		;else
		(and left_operand  (boolean-eval (car (cdr operands)))))
	   ) ;; end
;---or------ "or" expression check
	  ( 
	   (equal bool_operator 'or) ; bool_operator == or
				
	   (if (atom (car(cdr operands)))
	       ; if true
	        (or left_operand  (car(cdr operands)))
		;else
		(or left_operand  (boolean-eval (car (cdr operands)))))
	   ); or
;---ERROR------ "xor" expression check (using "boolean-xor" method)
	  ( 
	   (equal bool_operator 'xor) ; bool_operator == xor
				
	   (if (atom (car(cdr operands)))
	       ; if true
	        (boolean-xor left_operand  (car(cdr operands)))
		;else
		(boolean-xor left_operand  (boolean-eval (car (cdr operands)))))
	   ); xor

;---implies------ "implies" expression check (use "boolean-implies" method)
	  ( 
	   (equal bool_operator 'implies) ; bool_operator == implies
				
	   (if (atom (car(cdr operands)))
	       ; if true
	        (boolean-implies left_operand  (car(cdr operands)))
		;else
		(boolean-implies left_operand  (boolean-eval (car (cdr operands)))))
	   ); implies

;---iff------ "iff" expression check (use "boolean-implies" method)
	  ( 
	   (equal bool_operator 'iff) ; bool_operator == iff
				
	   (if (atom (car(cdr operands)))
	       ; if true
	        (boolean-iff left_operand  (car(cdr operands)))
		;else
	        (boolean-iff left_operand  (boolean-eval (car (cdr operands)))))
	   ); iff

	  
;----------------------------------------------------------------------	  
	  ) 
 ;______________________Sub cond ends here

	   
	 ) 	; length == 2 end

	  
	  )
;______________________Main cond ends here ______________



    ) ;; LET
 )  ;; DEFUN



