;; Return T if item is a member of set.

;; Return NIL if item is not a member of set.

;; The type of set is list.

;; Examples:

;;  (set-member '(1 2) 1) => T

;;  (set-member '(1 2) 3) =>  NIL

(defun set-member (set item)
	(cond
		((equal set nil) nil) ; Base case that sees if a set is empty then it returns nil
		((equal item (car set)) t) ; Return T if first atom is item
		(t (set-member (cdr set) item))) ; Calls this function recursively
)
(print '(----------------------------------------------------------------------------------------------------))
(print '(set member))
(print '(Is 1 a member of set (1 2)))
(print (set-member '(1 2) 1))
(print '(Is 1 a member of set (2 3)))
(print (set-member '(2 3) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the union of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;;   (set-union '(1 2) '(2 4)) => '(1 2 4)

(defun set-union (set-1 set-2)
   (if (equal set-1 nil)
      set-2  ; Base case if set-1 is empty return set-2
      (if (set-member set-2 (car set-1)) ; Checks if first atom is already in list-2
          (set-union (cdr set-1) set-2)  ; If it is, the fist atom in set-1 wont get looked at
          (set-union (cdr set-1) (cons (car set-1) set-2))))  ; Adds atom to set-2 if it isnt
)
(print '(----------------------------------------------------------------------------------------------------))
(print '(Set Union))
(print '(Union of sets (1 2) (2 4)))
(print (set-union '(1 2) '(2 4)))
(print '(Union of set (1 2) (3 4)))
(print (set-union '(1 2) '(3 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the intersection of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;;   (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)
 (if (equal set-1 '()) ;check if set-1 is empty (means empty intersection).
      '()
      (if (set-member set-2 (car set-1)) ;checks if the first element of set-1 is also a member of set-2
          (cons (car set-1) (set-intersection (cdr set-1) set-2)) ;first item in set-1 is a member of set-2, creates a new list, checks rest of list.
          (set-intersection (cdr set-1) set-2))) ;first item fo set-1 is not a member of set-2, check rest of list
)
(print '(----------------------------------------------------------------------------------------------------))
(print '(set intersection))
(print '(set intersection of (1 2) (2 4)))
(print (set-intersection '(1 2) '(2 4)))
(print '(set intersection of (1 2) (3 4)))
(print (set-intersection '(1 2) '(3 4)))
(print '(set intersection of (1 2 3) (2 3 4 5)))
(print (set-intersection '(1 2 3) '(2 3 4 5)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the difference of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;;

;; Examples:

;;   (set-diff '(1 2) '(2 4)) => '(1)

(defun set-diff (set-1 set-2)
 (if (equal set-1 '())
      '()
      (if (set-member set-2 (car set-1)) ;same as set-intersection
          (set-diff (cdr set-1) set-2) ;first element is a member of set-2, recursively call set-diff to check rest of list.
          (cons (car set-1) (set-diff (cdr set-1) set-2)))) ;first item(set-1) not a member of set-2 then make a list and continue to check through recursion.
)
(print '(----------------------------------------------------------------------------------------------------))
(print '(set difference))
(print '(set diff of (1 2) (2 4)))
(print (set-diff '(1 2) '(2 4)))
(print '(set diff of (1 2 3 4 5 6) (3 4 5 6 7)))
(print (set-diff '(1 2 3 4 5 6) '(3 4 5 6 7)))
(print '(set diff of (3 4 5 6) (1 2 3 4 5 6 7)))
(print (set-diff '(3 4 5 6 7) '(1 2 3 4 5 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the exclusive or of a and b

;;

;; Examples:

;;  (boolean-xor t nil) => t

;;  (boolean-xor nil nil) => nil

(defun boolean-xor (a b)
 (and (not (and a b)) (or a b))
)
(print '(----------------------------------------------------------------------------------------------------))
(print '(boolean-xor))
(print '(XOR of T T))
(print (boolean-xor t t))
(print '(XOR of T F))
(print (boolean-xor t nil))
(print '(XOR of F T))
(print (boolean-xor nil t))
(print '(XOR of F F))
(print (boolean-xor nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the implication of a and b

;;

;; Examples:

;;  (boolean-implies t nil) => nil

;;  (boolean-implies nil nil) => t

(defun boolean-implies (a b)
 (or (not a) b)
)
(print '(----------------------------------------------------------------------------------------------------))
(print '(boolean-implies))
(print '(implication of T T))
(print (boolean-implies t t))
(print '(implication of T F))
(print (boolean-implies t nil))
(print '(implication of F T))
(print (boolean-implies nil t))
(print '(implication of F F))
(print (boolean-implies nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the bi-implication (if and only if) of a and b

;;

;; Examples:

;;  (boolean-iff t nil) => nil

;;  (boolean-iff nil nil) => t

(defun boolean-iff (a b)
 (or (and a b) (and (not a) (not b)))
)
(print '(----------------------------------------------------------------------------------------------------))
(print '(boolean-iff))
(print '(bi-implication of T T))
(print (boolean-iff t t))
(print '(bi-implication of T F))
(print (boolean-iff t nil))
(print '(bi-implication of F T))
(print (boolean-iff nil t))
(print '(bi-implication of F F))
(print (boolean-iff nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluate a boolean expression.

;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.

;;

;; Examples:

;;  (boolean-eval '(and t nil)) => nil

;;  (boolean-eval '(and t (or nil t)) => t


;             8
;
; Evaluate a boolean expression.
; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
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
;---------------------------Base Cases----------------------------------------------------- 
      ( ; base cases: exp == t or exp = nil, boolean_operator == exp
      (or (equal (car exp) 't) (equal (car exp) 'nil)) (car exp)
      )

       ( ; Check for valid operator
       (not (set-member '(xor implies iff or not and) bool_operator))
       (format t "~%>>>>>> ERROR Invalid Operator: ~a"  bool_operator)
       )
;---------------------------Not Operator---------------------------------------------------
      
      (   ; length == 1
	   (and (equal operands_len 1) (equal bool_operator 'not)) 			
   ; (if (atom left_operand) (not left_operand) ( not(boolean-eval left_operand)))
  (if (set-member '(t nil) left_operand) (not left_operand) ( not(boolean-eval left_operand)))	   
	   )
    

;---------------------------Two Operands-----------------------------------------------------

      (	; length == 2
       (equal operands_len 2) ; condition to start sub cond
;______________________Sub cond starts here

       (cond

;--------------------------------------------------------------------------------
;---and------ "and" expression check     
          (  
	   (equal bool_operator 'and) ; bool_operator == and
	   ; (if (and (atom left_operand) (atom (car(cdr operands))))
	   (if (and (set-member '(t nil) left_operand) (set-member '(t nil) (car(cdr operands))))
	       (and left_operand  (car(cdr operands))) ; if true (Single operands on both sides)
	       (if (and (not (set-member '(t nil) left_operand)) (not(set-member '(t nil) (car(cdr operands)))))
	         (and  (boolean-eval left_operand)  (boolean-eval (car (cdr operands)))) ;Multiple  operands on both sides
	       (if(set-member '(t nil) left_operand)
		  (and left_operand  (boolean-eval (car (cdr operands)))) ; Left operand is single 
	   
		   (and (boolean-eval left_operand) (car (cdr operands) )) ; Right operand is single 
		  )
	       )
	       )

	   
	   ) ;; and

;---or------ "or" expression check
	  (  
	    (equal bool_operator 'or) ; bool_operator == or
	   (if (and (set-member '(t nil) left_operand) (set-member '(t nil) (car(cdr operands))))
	       (or left_operand  (car(cdr operands))) ; if true (Single operands on both sides)
	       (if (and (not (set-member '(t nil) left_operand)) (not(set-member '(t nil) (car(cdr operands)))))
	         (or  (boolean-eval left_operand)  (boolean-eval (car (cdr operands)))) ;Multiple  operands on both sides
	       (if(set-member '(t nil) left_operand)
		  (or left_operand  (boolean-eval (car (cdr operands)))) ; Left operand is single 
	   
		   (or (boolean-eval left_operand) (car (cdr operands) )) ; Right operand is single 
		  )
	       )
	       )
	    ) ;; or
	  
;---xor------ "exclusive or" expression check (uses "boolean-xor" method)
	  	  (  
	    (equal bool_operator 'xor) ; bool_operator == xor
	   (if (and (set-member '(t nil) left_operand) (set-member '(t nil) (car(cdr operands))))
	       (boolean-xor  left_operand  (car(cdr operands))) ; if true (Single operands on both sides)
	       (if (and (not (set-member '(t nil) left_operand)) (not(set-member '(t nil) (car(cdr operands)))))
	         (boolean-xor   (boolean-eval left_operand)  (boolean-eval (car (cdr operands)))) ;Multiple  operands on both sides
	       (if(set-member '(t nil) left_operand)
		  (boolean-xor  left_operand  (boolean-eval (car (cdr operands)))) ; Left operand is single 
	   
		   (boolean-xor  (boolean-eval left_operand) (car (cdr operands) )) ; Right operand is single 
		  )
	       )
	       )
	    ) ;; xor
		  
;---implies------ "implication" expression check (uses "boolean-implies" method)
	  	  	  (  
	    (equal bool_operator 'implies) ; bool_operator == implies
	   (if (and (set-member '(t nil) left_operand) (set-member '(t nil) (car(cdr operands))))
	       (boolean-implies  left_operand  (car(cdr operands))) ; if true (Single operands on both sides)
	       (if (and (not (set-member '(t nil) left_operand)) (not(set-member '(t nil) (car(cdr operands)))))
	         (boolean-implies   (boolean-eval left_operand)  (boolean-eval (car (cdr operands)))) ;Multiple  operands on both sides
	       (if(set-member '(t nil) left_operand)
		  (boolean-implies  left_operand  (boolean-eval (car (cdr operands)))) ; Left operand is single 
	   
		   (boolean-implies  (boolean-eval left_operand) (car (cdr operands) )) ; Right operand is single 
		  )
	       )
	       )
	    ) ;; implies

;---iff------ "bi-implication" expression check (uses "boolean-implies" method)
	   (
        (equal bool_operator 'iff) ; bool_operator == iff
	   (if (and (set-member '(t nil) left_operand) (set-member '(t nil) (car(cdr operands))))
	       (boolean-iff  left_operand  (car(cdr operands))) ; if true (Single operands on both sides)
	       (if (and (not (set-member '(t nil) left_operand)) (not(set-member '(t nil) (car(cdr operands)))))
	         (boolean-iff   (boolean-eval left_operand)  (boolean-eval (car (cdr operands)))) ;Multiple  operands on both sides
	       (if(set-member '(t nil) left_operand)
		  (boolean-iff  left_operand  (boolean-eval (car (cdr operands)))) ; Left operand is single 
	   
		   (boolean-iff  (boolean-eval left_operand) (car (cdr operands) )) ; Right operand is single 
		  )
	       )
	       )
	) ;; iff
	   
;----------------------------------------------------------------------	  
        
	  )
 ;______________________Sub cond ends here
	   
       ) 	; length == 2 end

      ; Invalid expression
      (t (format t "~%>>>>>> ERROR Invalid Expression ~a " exp))

	  )
;______________________Main cond ends here ______________


    ) ;; LET
 )  ;; DEFUN


;  (boolean-eval ' (and t (iff t (or nil (implies t (not nil)))))) == T

;  (boolean-eval '(implies (implies t t) (implies t nil))) = NIL
;  (boolean-eval '(and (and t (and t t)) (and (and t t) nil))) == NIL

; (boolean-eval '(iff (iff t t) (or nil (and t nil)))) == NIL


;(boolean-eval '(iff or (iff t t) (or nil (and t nil)))) == ERROR Invalid Expression

; (boolean-eval '(iffff (iff t t) (or nil (and t nil)))) == ERROR Invalid Operator				


