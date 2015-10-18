;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#10 and Chapter#10 Exercises
;Chapter#10 - What is the value of all of this

;from chap8.lisp
(defun build (a1 a2)
  (cons a1 (cons a2 '())))

(setf (symbol-function 'new-entry)   (function build))

;function to help in searching for name in names
;entry-f is the error function when name is not in names
;entry-f is called with name for entry-f to action accordingly
;( (key-1 key-2 key-3)
;( (val-1  (val-21 val-22) (val-31 val-32 val-33))))
; (lookup-in-entry key-2) => (val-21 val-22)
(defun lookup-in-entry-help (name names values entry-f)
  (cond
    ((null names) (funcall entry-f name))
    ((eq (car names) name) (car values))
    (t(lookup-in-entry-help
       name
       (cdr names)
       (cdr values)
       entry-f))))

(defun lookup-in-entry(name entry entry-f)
  (lookup-in-entry-help
   name
   (first entry)
   (second entry)
   entry-f)))

(defun entry-f (name)
  (cons name '(not found in list!!)))

(defparameter name 'entree)
(defparameter entry '((appetizer entree beverage) (food tastes good)))
;look for the name=entree in the first element of entry and return the 
;corresponding element in the second element of entry
(lookup-in-entry name entry (function entry-f)) ;tastes
(lookup-in-entry 'abc entry (function entry-f)) ;(abc not found in list)
(lookup-in-entry 'key-2 '((key-1 key-2 key-3) 
			  (val1 (val21 val22) (val31 val32 val33))) (function entry-f)) ;(val21 val22)


;mutual recursion between lookup-in-table and lookup-in-entry (and by extension lookup-in-entry-help_
;the mutual recursion happens via the entry-f parameter which is passed as a lambda with (cdr table)
;And this lambda is used when the element is not found in the search of the (car table) - the
;natural recursion i.e.
; table is of this form
;structure of table
;( (set_of_keys1) (list_of_values1)
;  (set_of_keys2) (lst_of_values2) )
(defun lookup-in-table(name table table-f)
  (cond
    ((null table) (funcall table-f name))
    (t (lookup-in-entry name 
			(car table) 
			(lambda (name) ;function when name is not found in (car table)
			  (lookup-in-table name (cdr table) table-f))))))
(defparameter name 'entree)
;structure of table
;( (set_of_keys1) (list_of_values1)
;  (set_of_keys2) (lst_of_values2) )
(defparameter table '(((entree dessert) (spaghetti spumoni))
		      ((appetizer entree beverage) (food tastes good))
		      ((key1_1 key1_2) (value1_1 value1_2))))
(lookup-in-table name table (function entry-f)) ;spaghetti
(lookup-in-table 'beverage table (function entry-f)) ;good
(lookup-in-table 'key1_1 table (function entry-f)) ;value1_1
(lookup-in-table 'key1_3 table (function entry-f)) ;(key1_3 not found in list)

(defparameter kvtable '( ((set1_key1 set1_key2 set1_key3) (set1_val1 (set1_val21 set1_val22) (set1_val31 set1_val32 set1_val33)))
			 ((set2_key1 set2_key2 set2_key3) (set2_val1 (set2_val21 set2_val22) (set2_val31)))
			 ((set3_key1 set3_key2 set3_key3) ((set3_val11 set3_val12) (set3_val21) (set3_val31 set3_val32)))))
;car is the first set of (key,value) pair
(car kvtable) ;((SET1_KEY1 SET1_KEY2 SET1_KEY3)
              ; (SET1_VAL1 (SET1_VAL21 SET1_VAL22) (SET1_VAL31 SET1_VAL32 SET1_VAL33)))
(lookup-in-table 'set2_key2 kvtable (function entry-f)) ;(set_val21 set2_val22)
(lookup-in-table 'set1_key2 kvtable (function entry-f)) ;(set1_val21 set1_val22)
(lookup-in-table 'set3_key2 kvtable (function entry-f)) ;(set3_val21)
(lookup-in-table 'set4_key1 kvtable (function entry-f)) ;(set4_key1 not found in list)

;from chap7.lisp
(defun 1st-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp(aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car aexp))

;added one line to return expression as-is
(defun value (aexp)
  (cond
    ((numberp aexp) aexp)
    ((eq (operator aexp) (quote plus)) (+ (value (1st-sub-exp aexp))
					  (value (2nd-sub-exp aexp))))
    ((eq (operator aexp) (quote times)) (* (value (1st-sub-exp aexp))
					   (value (2nd-sub-exp aexp))))
    ((eq (operator aexp) (quote expt)) (expt (value (1st-sub-exp aexp))
					     (value (2nd-sub-exp aexp))))
    (t aexp)))


(value (quote (car (quote (a b c)))))
(value '(add1 6)) ;(add1 6) - literal, this is not evaluated

;if e is a number, evaluate the number
;else return the value of e using *identifier
(defun atom-to-action (e)
  (cond
    ((numberp e) (function *self-evaluating))
    (t (function *identifier))))

;if e is an atom, use atom-to-action to retrieve the value
;else e is a list so, evaluate it via list-to-action
(defun expression-to-action(e)
  (cond
    ((atom e) (atom-to-action e))
    (t(list-to-action e))))

;based on what the (car e) is, call the appropriate function
;for evaluation of the e
(defun list-to-action (e)
  (cond
    ((atom (car e)) (cond
		      ((eq (car e) (quote quote))  (function *quote))
		      ((eq (car e) (quote lambda)) (function *lambda))
		      ((eq (car e) (quote cond))   (function *cond))
		      (t (function *application))))
     (t (function *application))))

;value of e using the function meaning
;meaning requires an 'environment' to evaluate e in, so in this case
;the environment is () since we are just getting the literal value of e
(defun value (e)
  (meaning e (quote ())))

;get the meaning of e in the context of environment table
;use the expression-to-action which returns the appropriate lambda
;and to that lambda pass the e and table as parameters
(defun meaning (e table)
  (funcall (expression-to-action e) e table))

;self-evaluating returns the expression e
(defun *self-evaluating (e table)  e)

;convenience: to call text-of-quotation as the second item of list
(setf (symbol-function 'text-of-quotation) (function second))

;(*quote (quote abc)) would return the (quote abc)
(defun *quote (e table) (text-of-quotation e))
(*quote '(quote abc) '()) ;ABC

;for the primitive functions - build them with the car as (quote primitive)
;user-defined functions will be marked as (quote non-primitive)
(defun *primitives(name)
  (cond
    ((eq (quote t) name) t)
    ((eq (quote nil) name) nil)
    (t (build (quote primitive) name))))

;e has been identified as an indentifier, so lookup the value of the identifier
;in the enviroment table. The table is the 'environment' that has keys and their values
(defun *identifier (e table)
  (lookup-in-table e table (function *primitives))) ;use *primitives to handle all the missing identifiers

(*identifier 'x '( ((x y) (1 2)) 
		   ((a b) (5 6)))) ;1
(*identifier 'b '( ((x y) (1 2)) 
		   ((a b) (5 6)))) ;6
(*identifier 'foo '( ((foo bar) ((car '(1 2 3))  (cdr '(5 6)))))) ;(car '(1 2 3))

;e is of the form (lambda (params) (body)), so *lambda
;conses the string non-primitive with the table (the environment) and
;the (cdr e) which is the lambda params and body
(defun *lambda (e table)
  (build
   (quote non-primitive)
   (cons table (cdr e))))
    
(defparameter e '(lambda (x) (cons x y)))
(defparameter table '(((y z) ((8) 9)))) ;set1 of values
(defparameter table2 '(((y z) ((8) 9)) ;set1 of values
		      ((foo bar) ((car '(1 2 3)) (cdr '(5 6)))))) ;set2 of values

;quite a lot happens in this
;meaning tries to find the meaning of the expression e in the context of table
;meaning checks what type e is - by checking the (car e)
;here e is a lambda, so list-to-action (via expression-to-action) will return the
; (function *lambda) since the (car e) is lambda
;Then meaning will funcall this (function *lambda) passing it the expression e and
;the table (the context i.e.) table
;The function *lambda then cons-es non-primitive with the table and the (cdr e)
;(cdr e) will remove the keyword lambda, returning the actual function implementation
;along with the parameter list of the lambda
(meaning e table) ;(NON-PRIMITIVE ((((Y Z) ((8) 9))) (X) (CONS X Y)))

;helper functions for evcon
(setf (symbol-function 'question-of) (function first))
(setf (symbol-function 'answer-of)   (function second))

;define the evaluation of the cond form - which evaluates each of its cond-lines
;checking if the first (left i.e.) is true and if so, evaluate the second (right i.e.)
;else check the next cond-line
;get the first form of the (car lines) and get its meaning
;if the meaning of this form is T, then get the meaning of the answer-of the (car lines) in
;the context of the environment (table i.e.)
;If the meaning of the question-of (car lines) is not T, then evcon with the (cdr lines)
(defun evcon (lines table)
  (cond
    ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
    (t (evcon (cdr lines) table))))

;helper for *cond
(setf (symbol-function 'cond-lines) (function cdr))
;the implementation for the cond
;need to use (cond-lines e) since (car e) == cond, and the condition starts from (cdr e)
(defun *cond (e table)
  (evcon (cond-lines e) table))
	      
(defparameter param_a '(cond (logical_1 logical_val11)
			     (logical_2 logical_val21)))

(defparameter param_b '(cond (logical_2 logical_val22)
			     (logical_1 logical_val12)))

;Note: logical_3, logical_val31 are not available in the environment
(defparameter param_c '(cond (logical_3 logical_val31)
			     (logical_1 logical_val12)))

;the value logical_val41 is not available in the environment
(defparameter param_d '(cond (logical_2 logical_val41)
		             (logical_1 logical_val41)))

;logical_val41 is not in the environment, but will not be evaluated
;the result for this should be valueOf12
(defparameter param_e '(cond (logical_2 logical_val41)
			     (logical_1 logical_val12)))

;note that the car and the cdr of the sets have to be of equal length
(defparameter param_env '( ((logical_1) (t)) ;set1
			   ((logical_2) (nil)) ;set2
			   ((logical_val11 logical_val12) (valueOf1 valueOf12)) ;set3
			   ((logical_val21 logical_val22) (valueOf21 valueOf22 valueOf23)))) ;set4
			  
(question-of (car (cdr param_a))) ;logical_1
(meaning 'logical_1 param_env) ;T
(answer-of (car (cdr param_a))) ;logical_val11
(meaning 'logical_val11 param_env) ;valueOf1
;and hence...
(*cond param_a param_env) ;valueOf1
(*cond param_b param_env) ;valueOf12
(*cond param_c param_env) ;Error: logical_val3 not found in list!!
;note that in this case the cdr of the car of param_d is not evaluated, since
;the meaning of the car returns nil
(*cond param_d param_env) ;Error: logical_val41 not found in the list!!
(*cond param_e param_env) ;valueOf12


(defparameter param_e     '(cond (coffee klatsch) (t party)))
(defparameter param_table '( ((coffee) (t)) ;set1
			     ((klatsch party) (5 (6))))) ;set2
(question-of (car (cdr param_e))) ;coffee
(meaning 'coffee param_table) ;T
(answer-of (car (cdr param_e))) ;klatsch
(meaning 'klatsch param_table);5
;and hence...
(*cond param_e param_table) ;5


;evaluate the list of args in the context of the table
;and return the meaning of these args
(defun evlis (args table)
  (cond
    ((null args) nil)
    (t (cons
	(meaning (car args) table) ;meaning of car
	(evlis   (cdr args) table))))) ;meaning of cdr

(defparameter l1 '(x y z))
(defparameter l2 '(a x foo))
(defparameter l3 '(p foo c))
(defparameter param_env '( ((x y z) (1 2 3))
			   ((a b c) (10 11 12))
			   ((foo bar baz) (x y z))))
(evlis l1 param_env) ;(1 2 3)
(evlis l2 param_env) ;(10 1 x)
(evlis l3 param_env) ;((p not found in list!!) X 12)

;helpers for *application
(setf (symbol-function 'function-of)   (function car))
(setf (symbol-function 'arguments-of)  (function cdr))

;function that will apply a function
;get the meaning of the car i.e. name of the function in the context of table
;get the meaning of the parameters of the function in the context of table
;apply the function to these evaluated arguments
(defun *application (e table)
  (*apply (meaning (function-of  e) table) ;name of function
	  (evlis   (arguments-of e) table))) ;arguments of function

;as we differentiate primitives (built-in functions) from
;non-primitives (user-defined functions), we need checking
;functions for both
;Recall that the *lambda form would tack the non-primitive to the
;car of the list => any function defined using the *lambda form is
;a non-primitive function
(defun primitive? (l)
  (eq (first l) (quote primitive)))
(defun non-primitive? (l)
  (eq (first l) (quote non-primitive)))

;this applies the function fun to the values vals
;based on whether this is a primitive or a non-primitive one,
;call the appropriate functions
(defun *apply (fun vals)
  (cond
    ((primitive? fun)     (apply-primitive (second fun) vals))
    ((non-primitive? fun) (apply-closure   (second fun) vals))))

;call the primitive functions when you see them in the name
;car will be the name of the function; second (and not cdr) the params
(defun apply-primitive (name vals)
  (cond
    ((eq (quote car)  name) (car  (first vals)))
    ((eq (quote cdr)  name) (cdr  (first vals)))
    ((eq (quote cons) name) (cons (first vals) (second vals)))
    ((eq (quote eq?)  name) (eq   (first vals) (second vals)))
    ((eq (quote atom) name) (atom (first vals)))
    ((eq (quote not)  name) (not  (first vals)))
    ((eq (quote null?) name) (null (first vals)))
    ((eq (quote number?) name) (numberp (first vals)))
    ((eq (quote zero?) name) (zerop (first vals)))
    ((eq (quote add1) name) (add1 (first vals)))
    ((eq (quote sub1) name) (sub1 (first vals)))))

;define helper functions that return the 3 parts of a function definition
;table (the environment i.e.), parameters, implementation (the body i.e.)
;note that the lambda* created a form like this
;(non-primitive (table) (lambda-params) (lambda-body))
(setf (symbol-function 'table-of)    (function first)) ;the environment
(setf (symbol-function 'formals-of)  (function second)) ;the parameters
(setf (symbol-function 'body-of)     (function third)) ;the lambda body

;define extend-table as a way to extend environment
;(defun extend-table ()
;  (function cons))
(setf (symbol-function 'extend-table) (function cons))

;closure is of this form
; ((table) (formals) (body))
; table ==  environment
; formals == parameters
;extend the table (the environment) by adding the parameters 
;of the lambda using (formals-of closure) to the environment using new-entry
;and consing it to the closure's environment - which you get using (table-of closure)
(defun apply-closure (closure vals)
  (meaning
   (body-of closure) ;body-of =>  function body
   (extend-table ;extend-table == cons
    (new-entry (formals-of closure) vals) ;new-entry == build == cons a1 a2;formals-of => params
    (table-of closure))))

(defparameter closure1 '( (((u v w) (1 2 3))
			   ((x y z) (4 5 6))) ;table for the closure i.e. env of the closure
			 (x y) ;params for closure
			 (cons z x))) ; body of the closure
(defparameter vals '((a b c) (d e f)))

(body-of closure1) ;(cons z x)
(defparameter closure-body (body-of closure1))
(formals-of closure1) ;(x y)
(new-entry (formals-of closure1) vals) ;((x y) ((a b c) (d e f)))
(table-of closure1) ;(((U V W) (1 2 3)) ((X Y Z) (4 5 6)))
(defparameter ext-env (extend-table (new-entry (formals-of closure1) vals)
				     (table-of closure1)))
;(((X Y) ((A B C) (D E F))) ((U V W) (1 2 3)) ((X Y Z) (4 5 6)))

(arguments-of closure1)
(evlis '(z x) ext-env) ;(6 (A B C))
(meaning 'cons ext-env) ;(primitive cons)

(meaning closure-body ext-env) ;(6 a b c)
;closure-body => (cons z x)
;ext-env => the large table listed in ext-env

(funcall (lambda (u v)
	    (lambda (b)
	      (cond
		(b u)
		(t v)))) '6 '(a b c))

;this doesn't work like exactly like CONS
;*cons takes two parameters u v
;returns a lambda that takes parameter b
;if b then return u i.e car, else return v i.e. cdr
(defun *cons(u v)
  (lambda (b)
    (cond
      (b u)
      (t v))))


(defparameter lunch (*cons 'apple '())) ;lunch is a lambda => #<FUNCTION :LAMBDA (B) (COND (B U) (T V))>
(funcall (*cons 'apple '()) t) ;apple

(defun *car (l)
  (funcall l t))
(defun *cdr (l)
  (funcall l nil))

(*car (*cons 'apple '()))
(*car lunch) ;apple
(*cdr lunch) ;nil
(funcall (funcall (*cons lunch lunch) t) t) ;apple

;Notes to remember
;1. in apply-primitive, we can just live with eq and atom now, since
; car and cdr were defined above (*car/*cdr). Though I am not 100% certain
; how the *car and *cdr are similar to the car and cdr
;2. The (defun) to define a function for the *application can also be done
; away, since we have the Y-combinator, which can create a recursion using
; function application

; And the killer is....
; Does that mean we can run the interpretor on the interpreter if we do the transformation with the Y-combinator?
; Yes, but don't bother !
; What this means is that the Y-combinator implemented interpreter can run itself - since all we are assuming is
; just eq and atom. So, the interpreter can run itself !!!
; It will take me a while (months / years ??) till I can explain *that one* clearly :)

;; ===============
;; Exercises
;; ===============

