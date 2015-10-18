;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#7 and Chapter#7 Exercises
;Chapter#7 - Shadows


(eq (quote a) 'a) ;T

; (x+3) is a S-exp. It structurally represents
; the expression we want to represent

; The representation of the arithmetic expression (1 + 3)
; is '(1 + 3). The first one is an evaluation, while the
; second one is the representation (S-exp) that will get evaluated

; Expression - (3 + (4 x 5))
; This function validates if the aexp is a valid arithmetic expression
; that contains +, x , ^ and numbers. This function can validate only
; the expressions with one nesting of arithmetic expressions. More than
; one, even though it returns T for those, it doesn't evaluate the expression.
; Reason is because the numbered? is not being recursed on the complete cdr
; but only on the (car (cdr (cdr aexp))) which gives only one sub-expression.
; To recurse completely, it should be recursed on (cdr) and the single case
; should check if the (car aexp) is numbered, recursing internally
; Case that will pass but should not
; (numbered? '(3 + (4 x 5) + (8 ^ 2) - (2 x 3)))
(defun numbered-1? (aexp)
  (cond
    ((atom aexp) (numberp aexp))
    ((eq (car (cdr aexp)) (quote +))
     (and
      (numbered-1? (car aexp))
      (numbered-1? (car (cdr (cdr aexp))))))
    ((eq (car (cdr aexp)) (quote x))
     (and
      (numbered-1? (car aexp))
      (numbered-1? (car (cdr (cdr aexp))))))
    ((eq (car (cdr aexp)) (quote ^))
     (and
      (numbered-1? (car aexp))
      (numbered-1? (car (cdr (cdr aexp))))))))
       

;simplifying numbered? since aexp is 'validated' to
;be an arithmetic expression
(defun numbered? (aexp)
  (cond
    ((atom aexp) (numberp aexp))
    (t(and
       (numbered? (car aexp))
       (numbered? (car (cdr (cdr aexp))))))))

;write numbered*? to check if aexp is a valid 
;numeric expression that uses +, x and ^
;Edge case - fails this one
; (numbered*? (2 + (3 4 )))
(defun numbered*? (aexp)
  (cond
    ((null aexp) t) ;this is t since nil list is anded for terminal condition
    ((atom (car aexp)) (cond
			 ((or (numberp (car aexp))
			      (eq (car aexp) (quote +))
			      (eq (car aexp) (quote *))
			      (eq (car aexp) (quote ^))) (and t (numbered*? (cdr aexp))))
			 (t nil))) ;short-circuit the evaluation
    ((not (atom (car aexp))) (and (numbered*? (car aexp))
				  (numbered*? (cdr aexp))))
    (t nil)))

(defparameter a1 '( (1 + 2) ^ 3 + (4 * 5) + (2 ^ 3) * (3 * 4)))
(numbered*? a1) ;T
(defparameter a2 '( (1 + 2) ^ 3 + (4 * 5) + (2 ^ 3) - (3 * 4)))
(numbered*? a2) ;nil, since - is not defined

;; ===============
;; The Eighth Commandment
;; Recur on all the subparts that are of the same nature:
;; - On all the sublists of a list
;; - On all the subexpressions of a represntation of an arithmetic expression
;; ===============

(defun value-1 (aexp)
  (cond
    ((numberp aexp) aexp)
    ((eq (car (cdr aexp)) (quote +)) ( + (value-1 (car aexp)) ;the key is the lookup. Check one value ahead of potential
					 (value-1 (car (cdr (cdr aexp))))));number. (car (cdr aexp)) for operator and car for operand
    ((eq (car (cdr aexp)) (quote *)) ( * (value-1 (car aexp))
					 (value-1 (car (cdr (cdr aexp))))))
    (t (expt (value-1 (car aexp))
	  (value-1 (car (cdr (cdr aexp))))))))

(value-1 '(3 + 4))
(defparameter y1 '((3 + 4) * ( 2 ^ 3)))
(value-1 y1)

(defun 1st-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp(aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car aexp))

(defun value (aexp)
  (cond
    ((numberp aexp) aexp)
    ((eq (operator aexp) (quote plus)) (+ (value (1st-sub-exp aexp))
					  (value (2nd-sub-exp aexp))))
    ((eq (operator aexp) (quote times)) (* (value (1st-sub-exp aexp))
					   (value (2nd-sub-exp aexp))))
    ((eq (operator aexp) (quote expt)) (expt (value (1st-sub-exp aexp))
					     (value (2nd-sub-exp aexp))))))

(defparameter a3 '(plus (times 3 6) (expt 8 2)))
(value a3) ;82

;; ===============
;; The Ninth Commandment
;; Use help functions to abstract from representations
;; ===============

(defun null-1? (s)
  (and (atom s)
       (eq '() s)))
  
(null-1? '())
(null-1? '(1 2 3))

(defun zero-1? (n)
  (null-1? n))

(zero-1? '()) ;zero is represented as ()
(zero-1? '(() ())) ;nil, as this is two

(defun add1+ (n)
  (cons '() n))

(add1+ '(())) ;(nil nil) i.e. 2
(add1+ '()) ;(nil) i.e. 1

(defun sub1- (n)
  (cdr n))

(defun sum+ (n m)
  (cond
    ((zero-1? m) n)
    (t (add1 (sum+ n (sub1- m))))))

; a number is either a zero or it is a one added to another number

(defun number? (n)
  (cond
    ((null n) t) ;empty list is a number
    (t (and
	(null-1? (car n)) ;car is () and 
	(number? (cdr n)))))) ; cdr is also ()

(number? '(() () ())) ;T
(number? '(() (1) ())) ; nil, since (1) is not a valid representation

;; ===============
;; Exercises
;; ===============

(defparameter aexp1 '(1 + (3 * 4)))
(defparameter aexp2 '((3 ^ 4) + 5))
(defparameter aexp3 '(3 * (4 * ( 5 * 6))))
(defparameter aexp4 5)
(defparameter l1 '())
(defparameter l2 '(3 + (66 6)))
(defparameter lexp1 '(and (or x y) y))
(defparameter lexp2 '(and (not y) (or u v)))
(defparameter lexp3 '(or x y))
(defparameter lexp4 'z)

;7.1
(defun mk+exp (aexp1 aexp2)
  (cons aexp1
	(cons (quote +)
	      (cons aexp2 '()))))
	 
(mk+exp '(1 + 2) '(a + b))
(mk+exp '1 3)

(defun mkopexp (op exp1 exp2)
  (cons exp1
	(cons op
	      (cons exp2 '()))))

(mkopexp '* '(1 + 2) '(c + d))
(mkopexp '/ '1 '3)

;the earlier operator function was for pre-fix operators
;Hence the function to get infix operators
(defun infixoperator (exp)
  (car (cdr exp)))

(defun validoperator? (exp)
  (cond
    ((eq exp '+) t)
    ((eq exp '*) t)
    ((eq exp '^) t)
    (t nil)))

;check the difference between this and 1st-sub-exp
;There it was (car (cdr exp)) since the operator was pre-fix
;In a prefix expression the first term is the first infix
(defun 1st-infix-sub-exp (exp)
  (car exp))

;This is the same as the 2nd-sub-exp since the second expression
;remains the same whether prefix or infix
(defun 2nd-infix-sub-exp (exp)
  (car (cdr (cdr exp))))

;7.2
;check if the exp is a valid arithmetic expression
;Do this by checking the return of either of the subexpressions' operator
(defun aexp? (exp)
  (cond
    ((null exp) nil)
    ((numberp exp) t)
    ((validoperator? (infixoperator exp)) (aexp? (1st-infix-sub-exp exp))
                                          (aexp? (2nd-infix-sub-exp exp)))
    (t nil)))

(aexp? aexp1) ;true
(aexp? aexp2) ;true
(aexp? l1) ;nil
(aexp? l2) ;nil
(aexp? '( (1 + 2) ^ (3 + 5) * (2 ^ 3))) ;true

;7.3
(defun count-op (aexp)
  (cond
    ((null aexp) 0)
    ((numberp aexp) 0)
    ((validoperator? (infixoperator aexp)) (+(+ 1 (count-op (1st-infix-sub-exp aexp)))
					     (count-op (2nd-infix-sub-exp aexp))))
    (t 0)))
       
       
(count-op aexp1) ;2
(count-op aexp3) ;3
(count-op aexp4) ;0

;7.4
(defun count-nums (aexp)
  (cond
    ((null aexp) 0)
    ((numberp aexp) 1) ;base case 
    ((validoperator? (infixoperator aexp)) ( + (count-nums (1st-infix-sub-exp aexp))
					       (count-nums (2nd-infix-sub-exp aexp))))
    (t 0)))
     
(count-nums aexp1) ;3
(count-nums aexp3) ;4
(count-nums aexp4) ;1

(defun non-atom (a)
  (not (atom a)))

;7.5
;Mutually-recurisve calls
;The caller should call aexp*? and never call valid-subexp*? directly. Here is why
;The aexp*? has the job of validating that every expression is a valid arithmetic expression
;It does this by checking if the car is an arithmetic operation or not. Once it sees that the
;car is an arithmetic operation, it entrusts the cdr to be parsed by the valid-subexpr*?
;valid-subexpr*? now has a cdr that doesn't have an operator, but could be a nested arithmetic
;expression. So, it checks if the input is an atom and if so, checks that both the values in the
;input are numbers. 
;If the input (from aexp*?, just to reiterate) is not an atom, then valid-subexpr*? dutifully gives this
;input to aexp*? asking it to validate if this is indeed an arithmetic expression.So, these two mutually
;recurse
(defun aexp*? (aexp)
  (cond
    ((null aexp) nil)
    ((validoperator? (car aexp)) (valid-subexp*? (cdr aexp)))
    (t nil)))

;Always to be called from aexp*?
(defun valid-subexp*? (aexp)
  (cond
    ((null aexp) t) ;this is super-important, since we are anding the result; stumped me for a while !
    ((non-atom (car aexp)) (and (aexp*? (car aexp))
				(valid-subexp*? (cdr aexp))))
    ((numberp (car aexp))  (valid-subexp*? (cdr aexp))) ;car will always be a number - else error
    (t nil)))


(aexp*? '(+ 2 3))
(aexp*? '(+ 2 3 4 5 ))
(aexp*? '(+ 2 (+ 3 4)))
(aexp*? '(* (+ 2 3) (+ 3 4)))
(aexp*? '(* (+ 2 3) (^ 3 4) (* 5 6) (+ 7 8) (* 9 10)))
(aexp*? '(* (2 3))) ;nil
(aexp*? '(+ ( 2 3) (^ 3 4) (- 5 6))) ;nil
(aexp*? '(+ (* a 2))) ;nil

;from chapter6.lisp
;check if a is part of l - l doesn't need to be a lat
(defun member* (a l)
  (cond
    ((null l) nil)
    ((non-atom (car l)) (or (member* a (car l))
			    (member* a (cdr l))))
    (t (cond
	 ((eq (car l) a) t)
	 (t (member* a (cdr l)))))))

(defparameter a 'chips)
(defparameter l '((potato) (chips ((with) fish) (chips))))
(member* a l)

(defun validbool? (exp)
  (cond
    ((eq 'and exp) t)
    ((eq 'or exp) t)
    ((eq 'not exp) t)
    (t nil)))

;7.7
;validate that all atoms in lat are in lexp
(defun covered? (lexp lat)
  (cond
    ((null lexp) t)
    ((null lat) nil)
    ((atom lexp) (member* lexp lat)) ;for the case where lexp is an atom instead of a lat
    ((atom (car lexp)) (cond
			 ((not (validbool? (car lexp))) (and (member* (car lexp) lat)
							     (covered? (cdr lexp) lat)))
			 (t (covered? (cdr lexp) lat))))
    ((non-atom (car lexp)) (and (covered? (car lexp) lat)
				(covered? (cdr lexp) lat)))
    (t nil)))

(covered? lexp1 l1) ;t
(covered? lexp2 l1) ;nil
(covered? lexp4 l1) ;t
    
;7.8
;alist is a list of pairs (a L-expression). First element is var and the second is value
;lookup var in the alist
(defun lookup (var alist)
  (cond 
    ((null alist) nil)
    ((non-atom (car alist)) (cond
			      ((eq (car (car alist)) var) (car (cdr (car alist))))
			      (t (lookup var (cdr alist))))) ;the cdr here is ((Y 0)) for l1
     (t nil)))
    
(defparameter l1 '((x 1) (y 0)))
(defparameter l2 '((u 1) (v 1)))
(defparameter l3 '())
(defparameter a 'y)
(defparameter b 'u)     

(lookup a l1) ;0
(lookup b l2) ;1
(lookup a l3) ;nl

;7.9
;lexp can be either a variable (a tuple i.e.) or a boolean equation
;with multiple variables
;ex: (defparameter lexp1 'x)
;    (defparameter lexp1 '(AND (OR X Y) Y)
;If a variable,then check if its value is 1 and if so, return true
;If lexp is an equation, then evaluate the values of the variables using
;the appropriate boolean operator and return the result
;Else return false
;Remember: 1st-sub-exp returns the first sub-expression and in this case
;that will be atoms in the boolean equation - and we lookup the values for 
; those atoms in the alist
(defun Mlexp (lexp alist)
  (cond
    ((null alist) nil)
    ((null lexp) nil)
    ((atom lexp) (cond ;lexp is an atom, check if its value is 1 in alist
		    ( (eq (lookup lexp alist) 1) t)
		    (t nil)))
    ((eq (operator lexp) 'AND) (and ;not an atom, so expression => get operator for evaluation
				(Mlexp (1st-sub-exp lexp) alist)
				(Mlexp (2nd-sub-exp lexp) alist)))
    ((eq (operator lexp) 'OR) (or
			       (Mlexp (1st-sub-exp lexp) alist)
			       (Mlexp (2nd-sub-exp lexp) alist)))
    ((eq (operator lexp) 'NOT) (not (Mlexp (1st-sub-exp lexp) alist)))
    (t nil)))

(Mlexp 'x '((x 1) (y 2))) ;T
(Mlexp 'x '((x 1))) ;T
(Mlexp 'y '((x 1))) ;NIL
(defparameter lexp1 '(AND x (OR x y)))
(defparameter lexp2 '(AND (OR x y) x))
(defparameter lexp3 '(AND x (AND (OR x y) z)))
(defparameter alist1 '((x 1) (y 0) (z 0)))
(defparameter alist2 '((x 1) (y 0) (z 1)))

(Mlexp lexp1 alist1) ;T (AND 1 (OR 1 0))
(Mlexp lexp2 alist1) ;T (AND (OR 1 0) 1)
(Mlexp lexp3 alist1) ;nil (AND 1 (AND (OR 1 0) 0))
(Mlexp lexp3 alist2) ;T (AND 1 (AND (OR 1 0) 1))


;7.10
;This function will evaluate a boolean expression of arbitary length (Mlexp was 2 boolean expressions)
;The function does this by evaluating the 1st sub-expression (base case) and for the cdr, checks if the
; (cdr (cdr lexp)) is not null i.e. there are atleast 2 variables to evaluate - for instance
; (AND x y z),the 1st-sub-exp will be x and then the (cdr (cdr lexp)) will be (y z) - and when this happens
; construct an operator-ed expression by cons-ing the right operator and recurse (general case).
; And when we are left with single operator - in the case of (AND Z), return true. The evaluation of Z happens
; via the 1st-sub-exp, so there isn't anything else to evaluate
; Pretty neat way - I think, of solving the problem !!
(defun Mlexp* (lexp alist)
  (cond
    ((null lexp) nil)
    ((null alist) nil)
    ((atom lexp) (cond
		   ((eq (lookup lexp alist) 1) t)
		   (t nil)))
    ((eq (operator lexp) 'AND) (and (Mlexp* (1st-sub-exp lexp) alist) ;param-1
				    (cond ;param-2
				      ((null (cdr (cdr lexp))) t)
				      (t (Mlexp* (cons 'AND (cdr (cdr lexp))) alist)))))
    ((eq (operator lexp) 'OR) (or (Mlexp* (1st-sub-exp lexp) alist) ;param-1
				    (cond ;param-2
				      ((null (cdr (cdr lexp))) t) ;for expressions of form (OR Z), this is nil, hence return
				      (t (Mlexp* (cons 'OR (cdr (cdr lexp))) alist))))) ;build an opertor-ed expression with CDR for parsing
    ((eq (operator lexp) 'NOT) (not (Mlexp* (1st-sub-exp lexp) alist))) ;not is a unary operator
    (t nil)))

(defparameter lexp4 '(AND x y z))
(Mlexp* lexp4 alist2) ;nil (AND 1 0 1)
(defparameter lexp5 '(AND x x z))
(Mlexp* lexp5 alist2) ;T (AND 1 1 1)
(defparameter lexp6 '(OR x y z))
(Mlexp* lexp6 alist2) ;T (OR 1 1 1)
(defparameter lexp7 '(AND x (OR y z) (AND x z)))
(Mlexp* lexp7 alist2) ;T (AND 1 (OR 0 1) (AND 1 1))
(defparameter lexp8 '(OR (Y Z) (AND X Y)))
(Mlexp* lexp8 alist2) ;T (OR (0 1) (AND 1 0))
(defparameter lexp9 '(AND (OR Y Z) (AND Y Z)))
(Mlexp* lexp9 alist2) ;NIL (AND (OR 0 1) (AND 0 1))
(defparameter lexp10 '(AND (NOT (OR Y Z)) (AND (NOT Y) (NOT Z))))
(Mlexp* lexp10 alist2) ;(AND (NOT (OR 0 1)) (AND (NOT 0) (NOT 1)))
;(AND (NOT 1) (AND 1 0))
;AND 0 1 => 0
(defparameter lexp11 '(OR (NOT (OR Y Z)) (AND (NOT Y) (NOT Z))))
(Mlexp* lexp11 alist2) ;same as above, instead (OR 0 1) => T




