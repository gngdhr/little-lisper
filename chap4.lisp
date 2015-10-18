;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#4 and Chapter#4 Exercises
;Chapter#4 - The Numbers Games

(1+ 2)
(1- 2)
(zerop 0)
(1+ 1)

;add two numbers
(defun myadd (n m)
  (cond
    ((zerop m) n)
    (t (myadd (1+ n) (1- m)))))

;another way to do addition
;as in the book
(defun myadd2 (n m)
  (cond
    ((zerop m) n)
    (t (1+ (myadd2 n (1- m))))))

; positive n and m only
; will fail if either n/m is negative
(defun mysub (n m)
  (cond
    ((zerop m) n)
    (t (mysub (1- n) (1- m)))))

;the subtraction as in the book
(defun mysub2 (n m)
  (cond
    ((zerop m) n)
    (t (1- (mysub2 n (1- m))))))

;; ===============
;; The fourth commandment
;; When recurring on a list of atoms, lat or a vec,
;; ask two questions about them, and use (cdr lat) or
;; (cdr vec) for the natural recursion
;; When recurring on a number, n, ask two questions, and
;; use (sub1 n) for the natural recursion
;; ===============

; sum a vector. Can use + instead of myadd
(defun addvec(vec)
  (cond ((null vec) 0)
	(t (myadd (car vec) (addvec (cdr vec))))))

(addvec '(1 2 3 4))

; multiply n and m; can replace + with myadd
(defun mymul (n m)
  (cond (
	 (zerop m) 0)
	 (t (+ n (mymul n (1- m))))))

(mymul 2 6)

;; ===============
;; The Fifth Commandment
;; When building a value with +, always use 0
;; for the value of the terminating line, for
;; adding 0 doesn't change the value of addition.
;; When building a value with x, always use 1 for
;; the value of the terminating line, for
;; multiplying by 1 doesn't change the value of the
;; multiplication.
;; When building a value with cons, always consider ()
;; for the value of the terminating line
;; ===============

(defun vecsum (vec1 vec2)
  (cond
    ((null vec1) vec2)
    ((null vec2) vec1)
    (t (cons
	(+ (car vec1) (car vec2))
	(vecsum (cdr vec1) (cdr vec2))))))

(vecsum '(1 2 3) '(4 5 6))

;sum vec1 and vec2
(defun vec+ (vec1 vec2)
  (cond
     ((null vec2) vec1)
     ((null vec1) vec2)
     (t(cons
	(+ (car vec1) (car vec2))
	(vec+ (cdr vec1) (cdr vec2))))))

(vec+ '() '(1 2 3))
(vec+ '(1 2 3) '())
(vec+ '(1 2 3) '(4 5))
(vec+ '(1 2) '(4 5 6))
(vec+ '(1 2) '(3 4))

; the nice thing about this > function is that
; by reordering the (zerop n) before (zerop m)
; the equality case i.e. (gt 3 3) is automatically 
; taken care of - which is pretty sweet! This happens
; because when n is zero, the function returns nil, since
; zero can't be greater than any other +ve number
(defun gt (n m)
  (cond
    ( (zerop n) nil)
    ( (zerop m) t)
    (t (gt (1- n) (1- m)))))

(gt '2 '3)
(gt '3 '2)
(gt '2 '2)

; while this will work for < case,
; it will fail when n == m
(defun lt1 (n m)
  (not (gt n m)))

(lt1 '2 '3)
(lt1 '3 '2)
(lt1 '2 '2)

; this would work for n=m too, returning nil
(defun lt (n m)
  (cond
    ( (zerop m) nil)
    ( (zerop n) t)
    (t (lt (1- n) (1- m)))))

(lt '2 '3)
(lt '3 '2)
(lt '2 '2)

;rewrite eq. I prefer eq1 over eq2
(defun eq1 (n m)
  (cond
    ( (gt n m) nil)
    ( (lt n m) nil)
    (t t)))

; the (zerop n) is required in case n < m
(defun eq2 (n m)
  (cond
    ( (zerop m) (zerop n))
    ( (zerop n) nil)
    (t (eq2 (1- n) (1- m)))))

; length of a lat
(defun len (lat)
  (cond
    ( (null lat) 0)
    (t (1+ (len (cdr lat))))))

(len '())
(len '(1 2 3))

; n ^ m
(defun pow (n m)
  (cond
    ((eq m 0) 1)
    (t(* n (pow n (1- m))))))

(pow 2 3) ;8
(pow 2 0) ;1

(defun pick-1 (n lat)
  (cond
    ((null lat) nil)
    ((zerop (1- n)) (car lat)) ;(zerop n) will give zero-index, we need one-index
    (t(pick-1 (1- n) (cdr lat)))))

(pick-1 4 '(lasagna spaghetti ravioli macaroni meatball)) ;macaroni
(pick-1 0 '(a)) ;nil
(pick-1 0 '()) ;nil
(pick-1 7 '(a b c))

;this will be inefficient in the case we iterate till n == 0
;to return the same (car lat) which is Nil
(defun pick-2 (n lat)
  (cond
    ((zerop (1- n)) (car lat))
    ((null lat) nil)
    (t (pick-2 (1- n) (cdr lat)))))
(pick-2 7 '(a b c))

; does the order of the terminating conditions matter?
; doesn't matter
(defun pick1 (n lat)
  (cond
    ( (null lat) nil)
    ( (zerop (1- n)) (car lat))
    ( t (pick1 (1- n) (cdr lat)))))

(defun rempick-1 (n lat)
  (cond
    ((null lat) nil)
    ((zerop (1- n)) (cdr lat)) ;one-index hence check (1- n) instead of (zerop n)
    (t(cons (car lat) (rempick-1 (1- n) (cdr lat))))))

(rempick-1 3 '(hotdogs with hot mustard)) ;(hotdogs with mustard)
(rempick-1 1 '(hotdogs with hot mustard)) ;(with hot mustard)
(rempick-1 0 '(hotdogs with hot mustard)) ;lat returned as-is

; remove atom# n in lat (1-index)
(defun rempick (n lat)
  (cond
    ( (null lat) nil)
    ( (zerop (1- n)) (cdr lat))
    (t (cons
	(car lat)
	(rempick (1- n) (cdr lat))))))

(defun non-num1 (lat)
  (cond
    ((null lat) nil)
    ((numberp (car lat)) (non-num1 (cdr lat)))
    (t(cons (car lat) (non-num1 (cdr lat))))))

(non-num1 '(5 pears 6 prunes 9 dates))

;remove all the numbers from a lat
(defun no-nums (lat)
  (cond
    ( (null lat) nil)
    (t (cond
	 ( (numberp (car lat)) (no-nums (cdr lat)))
	 (t (cons
	     (car lat)
	     (no-nums (cdr lat))))))))

(no-nums '(5 pears and 6 oranges))

;get a vector of numbers from a lat of atoms
(defun vecfromlat(lat)
  (cond
    ((null lat) nil)
    ((numberp (car lat)) (cons (car lat) (vecfromlat (cdr lat))))
    (t(vecfromlat(cdr lat)))))

(vecfromlat '(5 pears 6 prunes 9 dates)) ; (5 6 9)

;get a vec from a lat
(defun all-nums (lat)
  (cond
    ((null lat) nil)
    (t (cond
	 ( (numberp (car lat))
	    (cons (car lat) (all-nums (cdr lat))))
	 ( t (all-nums (cdr lat)))))))

(all-nums '(5 oranges and 6 pears))

(defun eqan-1 (a1 a2)
  (cond
    ((numberp a1) (cond
		    ((numberp a2) (= a1 a2))
		    (t nil))
     ((numberp a2) nil) ;a1 is not a number, but a2 is
     (t (eq a1 a2))))) ;a1 and a2 are not numbers=> atoms

; equate two atoms whether a number or otherwise
(defun eqan (a1 a2)
  (cond
    ((numberp a1)
         (cond ((numberp a2) (= a1 a2))
	       (t nil)))
    ((numberp a2) nil)
    (t (eq a1 a2))))

    
;; ===============
;; Exercises
;; ===============

(defparameter vec1 '(1 2))
(defparameter vec2 '(3 2 4))
(defparameter vec3 '(2 1 3))
(defparameter vec4 '(6 2 1))
(defparameter l '())
(defparameter zero 0)
(defparameter one 1)
(defparameter three 3)
(defparameter obj '(x y))

(defun duplicate (n obj)
  (cond 
    ((null obj) nil)
    ((zerop n) nil)
    (t(cons obj (duplicate (1- n) obj)))))

(duplicate three obj) ; ((x y) (x y) (x y))
(duplicate zero obj) ;nil
(duplicate one vec1) ; ((1 2))

(defun multvec (vec)
  (cond
    ((null vec) 1)
    (t(* (car vec) (multvec (cdr vec))))))
(multvec vec2) ;24
(multvec vec3) ;6
(multvec l) ;1

;((null vec) 1)

(defun index (a lat)
  (cond
    ((null lat) 0)
    ((eq (car lat) a) 1) ;one-index
    (t (+ 1 (index a (cdr lat))))))

(index 'car '(cons cdr car null? eq?)) ;3
(index 'car '(car engine auto motor)) ;1
(index 'motor '(car engine auto motor)) ;4

(defun vecproduct (vec1 vec2)
  (cond
    ((null vec1) vec2) ;when different lengths, return the other vector as-is
    ((null vec2) vec1)
    (t(cons (* (car vec1) (car vec2)) 
	    (vecproduct (cdr vec1) (cdr vec2))))))

(vecproduct vec1 vec2) ;(3 4 4)
(vecproduct vec2 vec3) ;(6 2 12)
(vecproduct vec3 vec4) ;(12 2 3)

(defun dot-product (vec1 vec2)
  (cond
    ((and (null vec1) (null vec2)) 0)
    (t(addvec (vecproduct vec1 vec2)))))

(dot-product vec2 vec2) ;29
(dot-product vec2 vec4) ;26
(dot-product vec3 vec4) ;17

(defun div (dividend divisor)
  (cond
    ((zerop dividend) 0)
    ((zerop divisor) NaN)
    ((> divisor dividend) 0)
    (t(+ 1 (div (- dividend divisor) divisor)))))

(div 7 5) ;1
(div 8 2) ;4
(div 2 3) ;0

(defun remainder (dividend divisor)
  (- dividend (* divisor (div dividend divisor))))

(remainder 7 5) ;2
(remainder 8 2) ;0
(remainder 2 3) ;2

(defun lte (lhs rhs)
  (cond
    ((<  (- lhs rhs) 0) t)
    ((eq (- lhs rhs) 0) t)
    (t nil)))

(lte zero one) ;T
(lte one one) ; T
(lte three one) ;nil
	 


