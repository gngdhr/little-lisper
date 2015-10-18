;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#9 and Chapter#9 Exercises
;Chapter# 9 - Lambda the Ultimate

;remove members in l based on condition test?
;l can be a lat or a list of S-exps
;Ref: http://cl-cookbook.sourceforge.net/functions.html
(defun rember-f (test? a l)
  (cond
    ((null l) nil)
    (t (cond ;the test? is assumed to be a function here and hence use funcall to call it
	 ((funcall test? (car l) a) (cdr l))
	 (t (cons (car l)
		  (rember-f test? a (cdr l))))))))

(defparameter l1 '(6 2 5 3))
(defparameter a1 '5)
;Read this: http://cl-cookbook.sourceforge.net/functions.html
;The function eq has to be explicitly marked as a function using the
;function operator (Lisp having separate namespaces for variables and functions)
(rember-f (function eq) a1 l1) ;(6 2 3)

(defparameter l2 '(jelly beans are good))
(defparameter a2 'jelly)       ;eq will check if the atoms are equal
(rember-f (function eq) a2 l2) ;(beans are good)

(defparameter l3 '(lemonade (pop corn) and (cake)))
(defparameter a3 '(pop corn))     ;equal - will check if the structure and the value of the form is equal (chap6.lisp)
(rember-f (function equal) a3 l3) ;(lemonade and (cake))

;returns a function that checks if the parameter passed to that function (x) is equal to the lambda returned with (c)
(defun eq?-c (a)
  (lambda (x) (eq x a)))

;define the function eq?-salad1 with the form (eq?-c 'salad)
;As mentioned here - http://cl-cookbook.sourceforge.net/functions.html, the car of the compound form that is eq?-salad1
;should be either a symbol or a lambda expression. But (eq?-c 'salad) is of type FUNCTION (which is the object type returned when we use defun)
;CL-USER> eq?-salad1
;#<FUNCTION :LAMBDA (X) (EQ X A)>
;So, we need to explicitly invoke the function using funcall as below
(defun eq?-salad1 (eq?-c 'salad))
(funcall eq?-salad1 'salad)
(funcall eq?-salad1 'tuna)
(eq?-salad1 'tuna) ;will fail
;Above will fail because the expanded function call will look like this
;#<FUNCTION EQ?-SALAD1 (EQ?-C QUOTE) --> Atleast that is what CLisp showed as outpu

;If we want to avoid the explicit call of funcall, we need to set the function cell
;of the compound form as the returned lambda. When we do (defun eq?-salad1 (eq?-c 'salad)),
;the returned lambda is stored in the value-cell of the compound form. We need that to be
;stored in the function-cell of the lambda so that we can call the function without needing
;to explicitly use funcall
;Ref: http://cl-cookbook.sourceforge.net/functions.html
(setf (symbol-function 'eq?-salad) (eq?-c 'salad))
(eq?-salad 'salad) ;t
(eq?-salad 'tuna) ;nil

;The earlier rember-f took 3 parameters - a function, the atom and the lat
;But, if we want a function that took only one parameter and returned another function which
;took the other 2 parameters and applied the original function, we need to rewrite rember-f
(defun rember-f1 (test?)
  (function 
   (lambda (a l)
    (cond 
      ((null l) nil)
      ((funcall test? (car l) a) (cdr l)) ;this has to be funcall test? since the assumption for evaluation is that test? is a symbol
      (t(cons (car l)
	      (funcall (rember-f1 test?) a (cdr l)))))))) 
;the last step is rather twisted
;(rember-f1 test?) would return a lambda as we expect
;so funcall this lambda, passing it two parameters, the a and the (cdr l) - the natural recursion
;and this will be consed to the case of t (as others)

(setq rember-eq? (rember-f1 (function eq)))
(funcall (rember-f1 (function eq)) 'tuna '(tuna salad is good)) ;works
(funcall rember-eq? 'tuna '(tuna salad is good)) ;this also works !

;this is failing - and that is very annoying - because the http://cl-cookbook.sourceforge.net/functions.html says
;that the DEFUN macro stores its parameters in the function-cell of the compound form. If it did that, then
;the funcall to rember-eq1 should succeed - but it doesn't X(
(defun rember-eq1 (rember-f1 (function eq)))
(rember-eq1 'tuna '(tuna salad is good)) ;returns nil - which is wrong - I expect this to work :(
(funcall (function rember-eq1) 'tuna '(tuna salad is good));returns nill - like the above
(funcall rember-eq1 'tuna '(tuna salad is good));fails saying rember-eq1 doesn't have a value
;Based on this - I think we should be using setq and not defun

(defparameter rember-eq2 (rember-f1 (function eq)))
(funcall rember-eq2 'tuna '(tuna salad is good));this works!
;makes sense since we are explicitly marking the car of the compound form as a function and funcall-ing it

(setf (symbol-function 'rember-eq3) (rember-f1 (function eq)))
(rember-eq3 'tuna '(tuna salad is good)) ;this works - again understandable, since we are setting the function cell explicitly

(defun rember-eq4 (rember-f1 eq))
(rember-eq4 'tuna '(tuna salad is good)) ;fails
(funcall rember-eq4 'tuna '(tuna salad is good));fails - could this be some sort of scope issue? http://stackoverflow.com/a/3857334

(funcall (rember-f1 (function eq)) 'tuna '(shrimp salad and tuna salad)) ;(shrimp salad and salad)
;note that something like this will not work
;(rember-f1 (function eq) 'tuna '(shrimp salad and tuna salad))
;saying invalid parameters since rember-f1 will take only one function - the test function
;That is why the get the return value of the rember-f1 and do a funcall on it with the two parameters

;insert to the left of the element once the test? passes
(defun insertL-f (test?)
  (function
   (lambda (old new l)
    (cond
      ((null l) nil)
      ((funcall test? (car l) old) (cons new (cons old (cdr l))))
      (t (cons (car l)
	       (funcall (insertL-f test?) old new (cdr l))))))))

(setq insertL-eq (insertL-f (function eq)))
(funcall insertL-eq 'tuna 'shrimp '(salad of tuna is good)) ;(salad of shrimp tuna is good)
(funcall insertL-eq 'tuna '(shrimp) '(salad of tuna is good)) ;(salad of (shrimp) tuna is good)

;insert to the right of the element once the test? passes
(defun insertR-f (test?)
  (function
   (lambda (old new l)
    (cond
      ((null l) nil)
      ((funcall test? (car l) old) (cons old (cons new (cdr l))))
      (t (cons (car l)
	       (funcall (insertR-f test?) old new (cdr l))))))))

(setq insertR-eq (insertR-f (function eq)))
(funcall insertR-eq 'tuna 'shrimp '(salad of tuna is good)) ;(salad of tuna shrimp is good)
(funcall insertR-eq 'tuna '(shrimp) '(salad of tuna is good)) ;(salad of tuna (shrimp) is good)

(defun seq-order-L (old new l)
  (cons new (cons old l)))
(defun seq-order-R (old new l)
  (cons old (cons new l)))

(defun insert-g (seq-order)
  (function
   (lambda (old new l)
    (cond
      ((null l) nil)
      ((eq (car l) old) (funcall seq-order old new (cdr l))) ;first argument to compound form should be lambda - explicitly marking seq-order as funcall
      (t(cons (car l)
	      ( funcall (insert-g seq-order) old new (cdr l)))))))) ;return of (insert-g seq-order) is the lambda - hence that becomes another form

(setq insert-g-L (insert-g (function seq-order-L)))
(funcall insert-g-L 'tuna 'shrimp '(salad of tuna is good)) ;(salad of shrimp tuna is good)
(setq insert-g-R (insert-g (function seq-order-R)))
(funcall insert-g-R 'tuna 'shrimp '(salad of tuna is good)) ;(salad of tuna shrimp is good)

;insert-g took a function that decided on the sequence order, but used eq internally for testing the (car l)
;insertL-f took a function that does the test of the function but decided the order of the cons
;These two are combined to create a generic insert-g-eq function that takes two functions
;1. First parameter:  a function that will do the equality check
;2. Second parameter: a function that will do the consing in the right order
;3. Third parameter: old 
;4. Fourth parameter: new
;5. Fifth parameter : the list to operate on
;So using this generic function, one can build partial or complete functions as required
;A partial function could be a function that does eq checks but lets the caller decide on the insert-L or insert-R
(defun insert-g-eq (test?)
  (function
   (lambda (seq-order)
    (function
     (lambda (old new l)
      (cond
	((null l) nil)
	((funcall test? (car l) old) (funcall seq-order old new (cdr l)))
	(t(cons (car l)
		(funcall (funcall (insert-g-eq test?) seq-order) old new (cdr l))))))))))

;This last line is awesome - and needs some explanation - took me sometime to get this working
;(funcall (funcall (insert-g-eq test?) seq-order) old new (cdr l))
;Let us traverse this outside-in first: The objective is to naturally recurse with (old new (cdr l))
;The insert-g-eq requires the test? function as the parameter and returns a lambda - so first get the
;lambda using (funcall (insert-g-eq test?) <something here>)
;The lambda returned by the above funcall takes a single parameter which is the seq-order and *that*
;lambda takes 3 parameters of (old new l)
;So funcall the returned lambda above with the parameter seq-order
;(funcall (funcall (insert-g-eq test?) seq-order) <something here>)
;The final <something  here> are the actual parameters
;(funcall (funcall (insert-g-eq test?) seq-order) old new (cdr l))

;The respective functions are created usign the same idea
;The insert-g-eq takes a (function eq) and returns a lambda
;Funcall that lambda with (function seq-order-L) to create the actual lambda
;The insert-g-eq-L actually expands to:
;<FUNCTION :LAMBDA (OLD NEW L)
;  (COND ((NULL L) NIL)
;   ((FUNCALL TEST? (CAR L) OLD) (FUNCALL SEQ-ORDER OLD NEW (CDR L)))
;   (T
;    (CONS (CAR L)
;     (FUNCALL (FUNCALL (INSERT-G-EQ TEST?) SEQ-ORDER) OLD NEW (CDR L)))))>

(setq insert-g-eq-L (funcall (insert-g-eq (function eq)) (function seq-order-L)))
(setq insert-g-eq-R (funcall (insert-g-eq (function eq)) (function seq-order-R)))
(funcall insert-g-eq-L 'tuna 'shrimp '(tuna salad is good)) ;(shrimp tuna salad is good)
(funcall insert-g-eq-R 'tuna 'shrimp '(tuna salad is good)) ;(tuna shrimp salad is good)
(funcall insert-g-eq-L 'tuna 'shrimp '(salad of tuna is good)) ;(salad of shrimp tuna is good)
(funcall insert-g-eq-R 'tuna 'shrimp '(salad of tuna is good)) ;(salad of tuna shrimp is good)
;same as above without the intermediate functions - I think this is rather clumsy and dirty looking :(
(funcall (funcall (insert-g-eq (function eq)) (function seq-order-L)) 'tuna 'shrimp '(tuna salad is good)) ;(shrimp tuna salad is good)
(funcall (funcall (insert-g-eq (function eq)) (function seq-order-L)) 'tuna 'shrimp '(salad of tuna is good)) ;(salad of shrimp tuna is good)
;using the #' macro instead of explicit function call
(funcall (funcall (insert-g-eq #'eq) #'seq-order-L) 'tuna 'shrimp '(tuna salad is good)) ;(shrimp tuna salad is good)

;from earlier chapters
(defun subst-1 (old new l)
  (cond
    ((null l) nil)
    ((eq (car l) old) (cons new (cdr l)))
    (t (cons (car l)  (subst-1 old new (cdr l))))))
(subst-1 'tuna 'shrimp '(salad of tuna is good)) ;(salad of shrimp is good)

(defun seq-subst (old new l)
  (cons new l))

;funcall the lambda returned by (insert-g-eq (function eq)) with the lambda returned by (function seq-subst)
;so this function takes the parameters that seq-subst would take
(setq insert-g-eq-subst (funcall (insert-g-eq (function eq)) (function seq-subst)))
(funcall insert-g-eq-subst 'tuna 'shrimp '(salad of tuna is good)) ;(salad of shrimp is good)

;Aside: observed something interesting regarding run-time modification of code
;Once setq is evaluated, it has the definition of the seq-subst within it. So, if seq-subst has an error
;then the insert-g-eq-subst would fail at runtime. And when you fix the seq-subst, those changes are not bound
;to the version that the insert-g-eq-subst holds. For these changes to take effect, you need to re-evaluate the
;(setq insert-g-eq-subst ..)

;xxx removes the a from l
;note that xxx uses the insert-g and not the overly-generic insert-g-eq. insert-g uses the eq function
;to check if the (car l) is equal to a. And if so, calls the function seqrem with old==a, new==nil and l==(cdr l)
;The seqrem function just returns the l (i.e. the (cdr l) thereby removing the old
(defun seqrem (old new l)  l)
(defun xxx ()
    (function
     (lambda (a l)
      (funcall (insert-g (function seqrem)) a  nil l))))

(funcall (xxx) 'sausage '(pizza with sausage and bacon)) ;(pizza with and bacon)

;; ===============
;; The Tenth Commandment
;; Abstract functions with common structures into a single function
;; ===============


;returns the system functions for +  * ^ based on operator
(defun atom-to-function (x)
  (cond
    ((eq x '+) #'+) ;need to use #' (in lieu of (function +) because
    ((eq x '*) #'*) ;the return value is not the symbol + but the 
    ((eq x '^) #'^))) ;function + (or * or ^)


(atom-to-function (operator '(+ 5 3 )))

;from chap7.lisp
(defun 1st-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp(aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car aexp))


(defun value (aexp)
    (cond
      ((numberp aexp) aexp)
      (t (funcall (atom-to-function (operator aexp))
		  (value (1st-sub-exp aexp))
		  (value (2nd-sub-exp aexp))))))

(value '(+ 5 3)) ;8
(value '(* (+ 2 3) 8)) ;40

;from chap8.lisp
(defun non-atom (a)
  (not (atom a)))

(defun member* (a l)
  (cond
    ((null l) nil)
    ((non-atom (car l)) (or (member* a (car l))
			    (member* a (cdr l))))
    (t (cond
	 ((eq (car l) a) t)
	 (t (member* a (cdr l)))))))


(defun subset? (set1 set2)
  (cond
    ((null set1) t)
    (t (and
	(member* (car set1) set2)
	(subset? (cdr set1) set2)))))


(defun intersect? (set1 set2)
  (cond
    ((null set1) t)
    (t (or
	(member* (car set1) set2)
	(intersect? (cdr set1) set2)))))


;begin abstraction creation
(defun set-f? (logical-operator const)
  (function
   (lambda (set1 set2)
    (cond
      ((null set1) const)
      (t (funcall logical-operator
	  (member* (car set1) set2)
	  (funcall (set-f? logical-operator const) (cdr set1) set2)))))))

;can't pass and and or as logical-operator for set-f? since these are operators
;and not functions. Hence defining functions that would do the operator's business
(defun and-prime (x y)
  (and x y))
(defun or-prime (x y)
  (or x y))

;define functions from the abstract function
(setq subset-?    (set-f? (function and-prime) t))
(setq intersect-? (set-f? (function or-prime)  t))
;these work
(funcall subset-? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) ;T
(funcall subset-? '(4 pounds of horseradish) '(four pinds chicken and 5 ounces horseradish)) ;nil

(and-prime nil (funcall subset-? '   (red wine tastes good) '(it goes well with brie cheese))) ;returns nil
(or-prime   t  (funcall intersect-? '(red wine tastes good) '(it goes well with brie cheese))) ;returns T

;build short-circuiting and-prime / or-prime with mutually recursing over 
;the respective calling functions
;This is done by the logical functions (and-prime-m / or-prime-m) calling their respective mutual recursion 
;(parent / public) functions with the cdr of the set1
;The public functions in-turn call member* with the set1 and the (cdr set2) to find if the condition holds
;And the condition being true/false is done by the logical-operator function
(defun and-prime-m (x set1 set2)
  (and x (funcall subset-m-? (cdr set1) set2)))
(defun or-prime-m (x set1 set2)
  (or x (funcall intersect-m-? (cdr set1) set2)))
;logical-operator: The function that will decide the branch to take
;const - the return value for the base condition return
;The mutual recursion works by this function calling the logical-operator
;with the (member*...) as the lambda, set1, set2 as the other parameters. 
;The logical-operator function ;decides on whether to return the value from
;the lambda or to call the recursion with the rest of the parameters
(defun set-f-m? (logical-operator const-for-null)
  (function
   (lambda (set1 set2)
    (cond
      ((null set1) const-for-null)
      (t (funcall logical-operator
		  (member* (car set1) set2) ;x in and-prime-m / or-prime-m
		  set1 ;set1 
		  set2)))))) ;set2

;define the new subset and intersect functions
(setq subset-m-?    (set-f-m? (function and-prime-m) t))
(setq intersect-m-? (set-f-m? (function or-prime-m) nil))

;few examples of usage
(or-prime-m (member* 'a '(a b c)) '(a b) '(a b c)) ;T
(funcall intersect-m-? '(a b) '(a b c)) ;T
(funcall intersect-m-? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) ;T
(funcall intersect-m-? '(4 pounds of horseradish) '(four pints chicken and 5 ounces)) ;nil
(funcall intersect-m-?  '(tomatoes and macaroni) '(ham or cheese)) ;nil
(funcall intersect-m-?  '(tomatoes and macaroni) '(macaroni and cheese)) ;T
(funcall subset-m-? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) ;T
(funcall subset-m-? '(4 pounds of horseradish) '(four pints chicken and 5 ounces)) ;nil


;the Mrember-curry defined using the standard commandments
(defun Mrember-curry-1 (l)
  (cond
    ((null l) nil)
    ((eq (car l) 'curry) (Mrember-curry-1 (cdr l)))
    (t (cons (car l)
	     (Mrember-curry-1 (cdr l))))))

;from chap5.lisp - removes multiple occurences of a in lat
(defun multirember (a lat)
  (cond ((null lat) nil)
	((eq (car lat) a) (multirember a (cdr lat)))
	(t(cons (car lat) (multirember a (cdr lat))))))

(defun Mrember-curry ()
  (function
   (lambda (l)
    (multirember 'curry l))))

;Mrember-curry returns a lambda that takes a list
;So, call the Mrember-curry using (Mrember-curry) and
;then funcall this returned lambda with the list parameter
(funcall (Mrember-curry) '(a b c curry e curry g curry)) ;(a b c e g)

(defun curry-maker (future)
  (function
   (lambda (l)
    (cond
      ((null l) nil)
      ((eq (car l) 'curry) (funcall (curry-maker future) (cdr l)))
      (t (cons (car l)
	       (funcall (curry-maker future) (cdr l))))))))
  
(defun Mrember-curry-2 ()
   (curry-maker 0))
(defun Mrember-curry-3 () ;this is what happens in the function-maker below
  (curry-maker (function curry-maker)))
;It really doesn't matter for curry-maker what the parameter passed is - in 
;both these cases the return value is the same

(funcall (Mrember-curry-2) '(my curry)) ;(my)
(funcall (Mrember-curry-2) '(a b c curry e curry f g curry)) ;(a b c e f g)
(funcall (Mrember-curry-3) '(my curry)) ;(my)
(funcall (Mrember-curry-3) '(a b c curry e curry f g curry)) ;(a b c e f g)

;The ahaa moment for this was that this function doesn't recurse on itself
;There is no recursive call to function-maker in this function
;This function recurses on the (future) passed to it - it uses itself (the function-maker)
;as the structure to call itself i.e. the natural recursion on finding curry is to 
;call the future (which is again itself - thanks to Mrember-curry-4 below) with the (cdr l)
;And when the eq is not true, it again calls itself - though not explicitly but via the
;future parameter which is a lambda holding function-maker with the (cdr l)
(defun function-maker (future)
  (function
   (lambda (l)
    (cond
      ((null l) nil)
      ((eq (car l) 'curry) (funcall (funcall future future) (cdr l))) ;need to do (funcall future future) since the first future is a lambda
      (t (cons (car l)
	       (funcall (funcall future future) (cdr l))))))))

(defun Mrember-curry-4 ()
    (function-maker (function function-maker)))

(funcall (Mrember-curry-4) '(a b c curry e curry f g curry)) ;(a b c e f g)

(defun add1-1 (x)
  (+ x 1))

(defun add1 ()
  (function
   (lambda (x)
    (+ x 1))))

;modified the function-maker to mark the (funcall (funcall future future) (cdr l)) as a 
;lambda that takes (arg) as the parameter and passign the (cdr l) as the parameter to this
;lambda
(defun function-maker-1 (future)
  (function
   (lambda (l)
    (cond
      ((null l) nil)
      ((eq (car l) 'curry) (lambda (arg) 
			     (funcall (funcall future future) arg)) (cdr l))
      (t (cons (car l)
	       (lambda (arg)
		 (funcall (funcall future future) arg)) (cdr l)))))))

;(defun function-maker-2 (future)
;  (lambda (future)
;     (M (lambda (arg)
;         ((future future) arg))))
(defun function-maker-2 (future)
  (function
   (lambda (recur)
    (lambda (l)
      (cond
	((null l) nil)
	((eq (car l) 'curry) (recur (cdr l)))
	(t (cons (car l) (recur (cdr l)))))))
   (lambda (arg)
    (future future) arg)))

(defun M (recur)
  (lambda (l)
    (cond
      ((null l) nil)
      ((eq (car l) 'curry) (recur (cdr l)))
      (t (cons (car l) (recur (cdr l)))))))

(defun function-maker (future)
  (M (lambda (arg)
       (funcall (future future) arg))))

;I still don't get this 100% - how this can do mrember of 'curry
(defun Mrember-curry ()
  ((lambda (future)
     (M (lambda (arg)
	  (funcall (future future) arg))))
   (lambda (future)
     (M (lambda (arg)
	  (funcall (future future) arg))))))

;fairly certain I still don't have my head wrapped around this one
;Other than the understanding that using this Y-combinator, one 
;can build recursive calls without the caller having to worry about
;the mechanics of the recursion
(defun Y ()
  (lambda (M)
    ((lambda (future)
       (M (lambda (arg)
	    (funcall (future future) arg))))
       (lambda (future)
	 (M (lambda (arg)
	      (funcall (future future) arg)))))))

(defun L ()
  (lambda (recur)
    (lambda (l)
      (cond
	((null l) 0)
	(t (add1 (recur (cdr l))))))))

(defun y-length (Y L))

;y-length using Y but not L
(defun y-length-1 ()
  (Y 
   (lambda (recur)
     (lambda (l)
       (cond
	 ((null l) 0)
	 (t (add1 (recur (cdr l)))))))))

;y-length without Y or L
(defun y-length-2 ()
  ((lambda (M)
     ((lambda (future)
	(M (lambda (arg)
	     (funcall (future future) arg))))
      (lambda (future)
	(M (lambda (arg)
	     (funcall (future future) arg))))))
   (lambda (recur)
     (lambda (l)
       (cond
	 ((null l) 0)
	 (t (add1 (recur (cdr l)))))))))

;proving a point, an expression that could get the length of a list
;without nothing but function application
(((lambda (M)
    ((lambda (future)
       (M (lambda (arg)
	    ((future future) arg))))
     (lambda (future)
       (M (lambda (arg)
	    ((future future) arg))))))
  (lambda (recur)
    (lambda (l)
      (cond
	((null l) 0)
	(t (add1 (recur (cdr l)))))))
  (quote a b c)))

;; ===============
;; Exercises
;; ===============

