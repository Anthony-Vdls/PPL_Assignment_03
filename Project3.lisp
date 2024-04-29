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

(defun boolean-eval (exp)

  

)