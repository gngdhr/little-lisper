;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#3 and Chapter#3 Exercises
;Chapter#3 - Cons the magnificient


; this rember function will not work since the
; atoms not being equal are never cons-ed to the remaining
; list
(defun rember-nowork (a lat)
  (cond
    ( (null lat) nil)
    ( t (cond
	  ( (eq (car lat) a) (cdr lat))
	  ( t (rember-nowork a (cdr lat)))))))

(rember-nowork 'toast '(bacon lettuce and tomato)) ;NIL, which is wrong
(rember-nowork 'and '(bacon lettuce and tomato)) ;tomato, which is wrong

;; ===============
;; The second commandment
;; Use cons to build lists
;; ===============

; function to remove atom a from the list of atoms, lat
(defun rember-1 (a lat)
  (cond
    ( (null lat) nil)
    ( t (cond
	  ( (eq (car lat) a) (cdr lat))
	  ( t ( cons
		(car lat)
		(rember-1 a (cdr lat))))))))

(defparameter l1 '(bacon lettuce and tomato))

; I find this version to be simpler than the
; rember1 since there is no need for the second
; always true condition
(defun rember-2 (a lat)
  (cond
    ( (null lat) nil)
    ( (eq (car lat) a) (cdr lat))
    ( (cons
       (car lat)
       (rember-2 a (cdr lat))))))
	      

; this is the suggested approach, so that the
; function's structure coincides with the
; data-structure of the data it is acting on
; The difference from rember-2 is the final catch-all 't'
; which does the consing of the car with cdr
(defun rember (a lat)
  (cond
    ( (null lat) nil)
    ( (eq (car lat) a) (cdr lat))
    ( t (cons
	 (car lat)
	 (rember a (cdr lat))))))

(rember 'and l1) ;(bacon lettuce tomato)

; assumption is that llat is a list of lats
; will fail for list of atoms i.e. just lats
(defun firsts (llat)
  (cond
    ( (null llat) nil)
    ( t (cons
	 (car (car llat))
	 (firsts (cdr llat))))))

(firsts '( (1 2) (3 4) (5 6))) ;(1 3 5)
;(firsts  '(1 2 3)) ; will fail

;; ===============
;; The Third Commandment
;; When building a list, describe the first typical element,
;; and then cons it onto the natural recursion
;; ===============

;notes on 'cond'
;cond is like a function that takes multiple parameters
;the first parameter has to be a boolean always
;there can be multiple conditions
;end the cond with a catch-all 't'
(defun insertR-1 (old new lat)
  (cond
    ((null lat) nil)
    (t(cond ((eq (car lat) old) (cons old (cons new (cdr lat))))
	    (t(cons (car lat) (insertR-1 old new (cdr lat))))))))

(defun insertR-2 (old new lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) old) (cons old (cons new (cdr lat))))
    (t (cons (car lat) (insertR-2 old new (cdr lat))))))

(defun insertR-3 (old new lat)
  (cond
    ((null lat) nil)
    (t (if (eq (car lat) old) (cons old (cons new (cdr lat)))
	   (cons (car lat) (insertR-3 old new (cdr lat)))))))
   
     
; insert new to the right of old in lat
; if there is no old, then return lat
(defun insertR (old new lat)
  (cond
    ( (null lat) nil)
    (t (cond
	 ( (eq (car lat) old)
	     (cons
	      old
	      (cons new (cdr lat))))
	 ( t (cons
	      (car lat)
	      (insertR old new (cdr lat))))))))

(insertR '1 '100 '(1 2 3 4 5))
(insertR '3 '314 '(1 2 3 4 5))
(insertR '4 '5 '(1 2 3))
(insertR 'topping 'fudge '(ice cream with fudge for dessert))

(defun insertL-1 (old new lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) old) (cons new lat))
    (t (cons (car lat) (insertL-1 old new (cdr lat))))))


; insert new to the left of old in lat
(defun insertL (old new lat)
  (cond
    ( (null lat) nil)
    (t (cond
	 ( (eq (car lat) old)
	    (cons
	      new
	      lat))
	 (t (cons
	     (car lat)
	     (insertL old new (cdr lat))))))))

(insertL '1 '0 '(1 2 3 4))
(insertL '2 '1 '(0 2 3 4))
(insertL '1 '2 ())

(defun subst-1 (old new lat)
  (cond ((null lat) nil)
	((eq (car lat) old) (cons new (cdr lat)))
	(t (cons (car lat) (subst-1 old new (cdr lat))))))


; substitute old with new in lat
; if there is no old, return lat
(defun subst1 (old new lat)
  (cond
    ( (null lat) nil)
    (t (cond
	 ( (eq (car lat) old)
	     (cons
	      new
	      (cdr lat)))
	 (t (cons
	     (car lat)
	     (subst1 old new (cdr lat))))))))
	 	  
(subst1 '3 '314 '(1 2 3 4 5))
(subst1 '1 '100 '(1 2 3 4 5))
(subst1 '1 '100 ())
(subst1 '1 '200 '(2 3 4))

;substitute either o1 or o2 in lat with new
(defun subst2 (o1 o2 new lat)
  (cond
    ((null lat) nil)
    ((or (eq (car lat) o1)
	 (eq (car lat) o2)) (cons new (cdr lat)))
    (t (cons (car lat) (subst2 o1 o2 new (cdr lat))))))

(subst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping))	 

	
;; ===============
;; Exercises
;; ===============

(defparameter l1 '((paella spanish) (wine red) (and beans)))
(defparameter l2 '())
(defparameter l3 '(cincinnati chili))
(defparameter l4 '(texas hot chili))
(defparameter l5 '(soy sauce and tomato sauce))
(defparameter l6 '((spanish) () (paella)))
(defparameter l7 '((and hot) (but dogs)))
(defparameter a1 'chili)
(defparameter a2 'hot)
(defparameter a3 'spicy)
(defparameter a4 'sauce)
(defparameter a5 'soy)
 
(defun seconds (llat)
  (cond
    ((null llat) nil)
    (t (cons (car (cdr (car llat))) (seconds-1 (cdr llat))))))

(seconds l1) ;(spanish red beans)
(seconds l2) ;nil
(seconds l7) ;(hot dogs)

;iterate over the list l and cons a as many times
;nice and simple :)
(defun dupla (a l)
  (cond
    ((null l) nil)
    (t(cons a (dupla a (cdr l))))))

(dupla a2 l4) ;(hot hot hot)
(dupla a2 l2) ;nil
(dupla a1 l5) ;(chili chili chili chili chili)

(defun double (a lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) a) (cons a lat))
    (t (cons (car lat) (double a (cdr lat))))))

(double a2 l2) ;nil
(double a1 l3) ;(cincinati chili chili)
(double a2 l4) ;(texas hot hot chili)

(defun subst-sauce (a l)
  (cond 
    ((null l) nil)
    (t (subst-1 'sauce a l))))

(subst-sauce a1 l4) ;(texas hot chili)
(subst-sauce a1 l5) ;(soy chili and tomato sauce)
(subst-sauce a4 l2) ;nil


(defun subst3 (new o1 o2 o3 lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) o1) (cons new (cdr lat)))
    ((eq (car lat) o2) (cons new (cdr lat)))
    ((eq (car lat) o3) (cons new (cdr lat)))
    (t (cons (car lat) (subst3 new o1 o2 o3 (cdr lat))))))


(subst3 a5 a1 a2 a4 l5) ; (soy soy and tomato sauce)
(subst3 a4 a1 a2 a3 l4) ; (texas sauce chili)
(subst3 a3 a1 a2 a5 l2) ; nil

;return t if a is part of lat else nil
(defun is-member? (a lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) a) t)
    (t (is-member? a (cdr lat)))))

;search for the first atom that is common between slat and lat
; and then replace that atom in lat with new
(defun substN (new slat lat)
  (cond
    ((null lat) nil)
    ((is-member? (car lat) slat) (subst-1 (car lat) new lat))
    (t (cons (car lat) (substN new slat (cdr lat))))))

(substN a2 l3 l4) ;(texas hot hot)
(substN a4 l3 l5) ;(soy sauce and tomato sauce)
(substN a4 l3 l2) ;()

;once car of lat == a, check if a is available in cdr lat and if so,
;remove a from the cdr only. Return lat as is if there aren't 2 occurences of a
(defun rember2 (a lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) a) (cond 
			((is-member? a (cdr lat)) (cons a (rember a (cdr lat))))
			(t lat)))
    (t (cons (car lat) (rember2 a (cdr lat))))))
    
(rember2 a1 l3) ;cincinati chili
(rember2 a4 l5) ;soy sauce and tomato
(rember2 a4 l2) ;nil
