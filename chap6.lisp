;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#6 and Chapter#6 Exercises
;Chapter#6 - *Oh my Gawd* It's full of stars


; return the leftmost atom of the lat
(defun leftmost (lat)
  (cond
    ((null lat) nil)
    (t (cond
	 ( (atom (car lat)) (car lat))
	 ( t (leftmost (car lat)))))))

(leftmost '(1 2 3))
(leftmost '((1) 2 3))
(leftmost '(((() four)) 17 (seventeen)))

(defun non-atom (s)
   (not (atom s)))

(non-atom 's)
(non-atom '(1 2 3))

(defun not1 (s)
  (cond
    (s nil) ;if s is true, then return nil
    (t t))) ;everything else return true

;remove a from l, where a can be list of atoms or 
;l could be list of list of atoms (and so on)
(defun rember*-1 (a l)
  (cond 
    ((null l) nil)
    (t (cond
	 ((atom (car l)) (cond
			   ((eq (car l) a)  (rember*-1 a (cdr l)))
			   (t (cons (car l) (rember*-1 a (cdr l))))))
	 (t(cons (rember*-1 a (car l)) ;need to search within the non-atom car and then cons
		 (rember*-1 a (cdr l)))))))) ;with cdr after searching in cdr

(rember*-1 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember*-1 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

; A tad difficult to get your head around for the first time
; Check if the (car l) is an atom, if so, is it equal to a, if so
; iterate the (cdr l) using rember*, removing the (car l)
; If the (car l) is not equal to a, cons it with the
; search result of the cdr - this is important, since we want to ensure
; that we are searching for a within the cdr
; And if (car l) is not an atom, recurse over the (car l) till
; we find an atom and cons it with the (cdr l) - searching for a
; in both the car and also in cdr - since a could be in either of the
; non-atoms
; And in case you are wondering why leftmost can't be used here to
; check for equality with a - it is because, even if we did find the
; leftmost element, we need to ensure that the structure of the non-atom
; is preserved. With leftmost, we will get the leftmost atom, and we will
; have to backtrack to find the list to get the cdr - which is inefficient
(defun rember* (a l)
  (cond
       ( (null l) nil)
       ( (non-atom (car l))
	    (cons
	        (rember* a (car l))
		(rember* a (cdr l))))
       (t (cond
	    ( (eq (car l) a)
	             (rember* a (cdr l)))
	    (t (cons
		   (car l)
		   (rember* a (cdr l))))))))
		  
(rember* '1 '( (2 3 4) 1 (1) (1 2 3) (4 5 6) (((1)2)3))); ((2 3 4) NIL (2 3) (4 5 6) ((NIL 2) 3))
(rember* '1 '( (2 3 4 1) 1 (1 4 5) ((1)))) ;((2 3 4) (4 5) (NIL))
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

;insert new to the right of old wherever old occurs in l
;l is not guaranteed to be only list of atoms, can have list of lists
(defun insertR* (old new l)
  (cond
    ((null l) nil)
    ((non-atom (car l)) (cons (insertR* old new (car l))
			      (insertR* old new (cdr l))))
    (t (cond
	 ((eq (car l) old) (cons old (cons new (insertR* old new (cdr l)))))
	 (t (cons (car l) (insertR* old new (cdr l))))))))

(defparameter old 'chuck)
(defparameter new 'roast)
(defparameter l '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
(insertR* old new l)


;; ===============
;; The Fourth Comandment (revised)
;; When recurring on a list of atoms, lat, or a vec, vec, ask two questions about them,
;; and use (cdr lat) or (cdr vec) for the natural recursion [These two questions are null (or) otherwise]
;; When recurring on a list of S-expressions, l, ask three questions:
;; (null? l), (atom? (car l)) and (non-atom? (car l)) 
;; and use (car l) and (cdr l) for the natural recursion [note: use both for natural recursion]
;; When recurring on a number, n, ask two questions and use (sub1 n) for the natural recursion
;; [the two questions are is it zero or non-zero]
;; ===============

;return the # of occurences of a in l
;Check the similarity of the structure of this function and the insertR* above
;They are identical  and the pattern is as follows
; 1. Check if null, if so return nil / zero
; 2. Check if car is non-atom, if so
; 2.1 Add1 / Cons the recursion of (car l) with the (cdr l)
; 3. (car l) is atom, hence check if (car l) == a
; 3.1 If so, add1 / cons new and recurse over (cdr l)
; 3.2 Not equal, so add 0 / cons (car l) and recurse over (cdr l)
(defun occur* (a l)
  (cond
    ((null l) 0)
    ((non-atom (car l)) (+ (occur* a (car l))
			   (occur* a (cdr l))))
    (t (cond
	 ((eq (car l) a) ( + 1 (occur* a (cdr l))))
	 (t (+ 0 (occur* a (cdr l)))))))) ;in the book (+ 0) is not done, rather just recurse over (cdr l)

(defparameter a 'banana)
(defparameter l '((banana) (split ((((banana ice))) 
				   (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
(occur* a l) ;5

;substitute all occurences of old with new in l
;The structure of the function remains the same as earlier !
;It is a learning experience to just look at function structures to see what they do !
;And this is where, I think the origin of higher order funtions like foldl/foldr
;arise - where there are HOFs working on the structure and the caller telling what
;each of the sub-structures are
(defun subst* (old new l)
  (cond
    ((null l) nil)
    ((non-atom (car l)) (cons (subst* old new (car l))
			      (subst* old new (cdr l))))
    (t (cond
	 ((eq (car l) old) (cons new (subst* old new (cdr l))))
	 (t (cons (car l) (subst* old new (cdr l))))))))

(defparameter old 'banana)
(defparameter new 'orange)
(subst* old new l)

;insert new to the left of all occurences of old
;note that in the last form, where (car l) != old, there is no
;use of consing the (car l) to the result of recursing on (cdr l)
;I did that just for completeness sake. The same could be wrtitten as
; (t (insertL* old new (cdr l))) - which is what the book has
(defun insertL* (old new l)
  (cond
    ((null l) nil)
    ((non-atom (car l)) (cons (insertL* old new (car l))
			      (insertL* old new (cdr l))))
    (t (cond
	 ((eq (car l) old) (cons new (cons (car l) (insertL* old new (cdr l)))))
	 (t (cons (car l) (insertL* old new (cdr l))))))))

(defparameter old 'chuck)
(defparameter new 'pecker)
(defparameter l '((how much (wood)) could (( a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
(insertL* old new l)

;check if a is a member of l
;In this function, I didn't bother consing the car in the last form,
;just recursed over the (cdr l)
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

;member* function without non-atom, using atom
(defun member** (a l)
  (cond
    ((null l) nil)
    ((atom (car l)) (cond
		      ((eq (car l) a) t)
		      (t(member** a (cdr l)))))
    (t(or (member** a (car l))
	  (member** a (cdr l))))))

(member** a l)

(and (atom (car '(mozzarella pizza)))
	   (eq (car l) 'pizza)) ;nil
(and (atom (car '((mozzarella mushroom) pizza)))
     (eq (car '((mozzarella mushroom) pizza)) 'pizza)) ;nil

;eqan checks if two atoms are equal
;From chap4.lisp
(defun eqan? (a1 a2)
  (cond
    ((numberp a1)
         (cond ((numberp a2) (= a1 a2))
	       (t nil)))
    ((numberp a2) nil)
    (t (eq a1 a2))))

;this is a bit complicated function - that checks two lists are structurally and content-wise equal
; 1. If both are null lists, return t
; 2. If the above failed, and one of them is a null, then return nil (nil != lat)
; 3. If both the list cars are non-atom, then eqlist is a conjunction of eqlist for the cars and the
; natural recursion of the cdrs i.e., recurse over cars for both and see if they are equal and then 
; recurse over the cdrs to do so too
; 4. If one of the lists's car is a non-atom (since #3 took care of both non-atom), then return nil. This
; takes care of checking the structure of the lists being equal
; 5. Finally, the base case of checking if the cars of l1 and l2 are equal and do that by checking the eqan
; of the cars and naturally recurse over the cdrs to check the rest of the list - and the result is the 
; conjunction of these two results
(defun eqlist? (l1 l2)
  (cond ((and (null l1)
	      (null l2)) t)
	((or  (null l1) ;since and is the first condition null & null condition is handled
	      (null l2)) nil)
	((and (non-atom (car l1))
	      (non-atom (car l2))) (and (eqlist? (car l1) (car l2))
					(eqlist? (cdr l1) (cdr l2))))
	((or (non-atom (car l1)) ;again, both non-atom taken care in the above condition
	     (non-atom (car l2))) nil)
	(t (and (eqan (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2))))))
	  
(eqlist? '(strawberry ice cream) '(strawberry ice cream)) ;t
(eqlist? '(strawberry ice cream) '(strawberry cream nice)) ;nil
(eqlist? '(banana (split)) '((banana) (split))) ;nil
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) ;nil
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) ;t

;This function calls eqlist? above to check if the list contents are the same
;There is redundancy in the checks ! And that is removed next
(defun equal? (s1 s2)
  (cond
    ((and (atom s1)
	  (atom s2)) (eqan? s1 s2))
    ((and (non-atom s1)
	  (non-atom s2)) (eqlist? s1 s2))
    (t nil)))

;check if the s1 and s2 are atoms, if so, call eqan
;if they are not, then call eqlist?? which will handle
;the S-exp traversal and then call equal?? when needed
(defun equal?? (s1 s2)
  (cond
    ((and (atom s1)
	  (atom s2)) (eqan? s1 s2))
    ((and (non-atom s1)
	  (non-atom s2)) (eqlist?? s1 s2))
    (t nil)))

;calls equal?? which calls this eqlist??
;mutual recursion
(defun eqlist?? (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or  (null l1) (null l2)) nil)
    (t (and
	(equal?? (car l1) (car l2))
	(eqlist?? (cdr l1) (cdr l2))))))

(eqlist?? '(strawberry ice cream) '(strawberry ice cream)) ;t
(eqlist?? '(strawberry ice cream) '(strawberry cream nice)) ;nil
(eqlist?? '(banana (split)) '((banana) (split))) ;nil
(eqlist?? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) ;nil
(eqlist?? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) ;t

;this is a * star function i.e. removes all the occurences of s in l
(defun remberS-1 (s l)
  (cond 
    ((null l) nil)
    ((non-atom (car l)) (cond
			  ((equal (car l) s) (remberS-1 s (cdr l)))
			  (t(cons (car l) (remberS-1 s (cdr l))))))
    (t(cond
	((eq (car l) s) (remberS-1 s (cdr l)))
	(t(cons (car l) (remberS-1 s (cdr l))))))))

;this isn't a * function, since, it removes only the first occurence of s
;by returning (cdr l)
(defun remberS (s l)
  (cond 
    ((null l) nil)
    ((eq (car l) s) (cdr l))
    (t(cons (car l) (remberS s (cdr l))))))

;the insertL* function is already fairly simplified, the only difference from
;the code in the book is that there is no (t) condition and the rest of the
;checks are done as part of the (cond

;; ===============
;; The Seventh Commandment
;; Simplify only after the function is correct
;; ===============


;; ===============
;; Exercises
;; ===============

(defparameter l1 '((fried potatoes) (baked (fried)) tomatoes))
(defparameter l2 '(((chili) chili (chili))))
(defparameter l3 '())
(defparameter lat1 '(chili and hot))
(defparameter lat2 '(baked fried))
(defparameter a 'fried)

(defun down* (l)
  (cond
    ((null l) nil)
    ((atom (car l)) (cons (cons (car l) nil) ;convert the atom to a lat
			  (down* (cdr l)))) ;cons with the result of the cdr - (cons lat1 lat2)
    ((non-atom (car l)) (cons (down* (car l)) ;get the car and natural recurse with cdr
			      (down* (cdr l))))))

(down* l2) ;((((CHILI)) (CHILI) ((CHILI))))
(down* l3) ;nil
(down* lat1) ;((CHILI) (AND) (HOT))
(down* l1) ;(((FRIED) (POTATOES)) ((BAKED) ((FRIED))) (TOMATOES))

;return the # of occurences in l of the common elements of
;lat and l 
(defun occurN* (lat l)
  (cond ((or (null lat)
	     (null l)) 0)
	(t (+ (occur* (car lat) l)
	      (occurN* (cdr lat) l)))))

(occurN* lat1 l2) ;3
(occurN* lat2 l1) ;3
(occurN* lat1 l3) ;0

;double the # of a in l
;check if a is a member of l and if so
; insertR a to a of l
;else return l
(defun double* (a l)
  (cond
    ((null l) nil)
    ((member* a l) (insertR* a a l))
    (t l)))

(double* a l1) ;((FRIED FRIED POTATOES) (BAKED (FRIED FRIED)) TOMATOES)
(double* a l2) ;(((CHILI) CHILI (CHILI)))
(double* a lat2) ;(BAKED FRIED FRIED)

;lat? has to ask 3 questions since the function could be passed a list
;of S-expressions. So, there are 3 things to check
;is the list empty, is the car an atom or anything else
;And lat doesn't need to recur on the car, since once the car is an atom
;there isn't anything else to check

(member* 'chips '((potato) (chips ((with) fish) (chips)))) ;T

;searching a in l by starting at the end of l
(defun member*** (a l)
  (cond
    ((null l) nil)
    ((non-atom (car l)) (or (member*** a (cdr l)) ;search cdr before searching car
			    (member*** a (car l))))
    (t (or (member*** a (cdr l)) ;found atom, but keep searching in cdr
	   (eq (car l)a))))) ;finished with the cdr, check if atom == a

(member*** 'chips '((potato) (chips ((with) fish) (chips)))) ;T

;sum all numbers in l; l is not necessarily a lat
(defun list+ (lvec)
  (cond
    ((null lvec) 0)
    ((non-atom (car lvec)) (+ (list+ (car lvec))
			      (list+ (cdr lvec))))
    (t(cond
	((numberp (car lvec)) (+ (car lvec) 
				 (list+ (cdr lvec))))
	(t (list+ (cdr lvec)))))))

(defparameter l1 '((1 (6 6 ()))))
(defparameter l2 '((1 2 (3 6)) 1))
(list+ l1) ;13
(list+ l2) ;13
(list+ l3) ;0

;g* function sums the the numbers in lvec using an accumulator - acc
;fyi: doesn't check that lvec has only numbers; that is assumed
;Also, lvec is a lvec i.e. only numbers can't have non-atoms
(defun g* (lvec acc)
  (cond 
    ((null lvec) acc)
    ((atom (car lvec)) (g* (cdr lvec) (+ (car lvec) acc)))
    (t (g* (car lvec)  (g* (cdr lvec) acc)))))

(g* '(1 2 3 4 5 6) 0) ;21

(defun member? (a lat)
  (cond
    ((null lat) nil)
    ((eq a (car lat)) t)
    (t (member? a (cdr lat)))))

;reverses a l by removing duplicates
(defun f* (l acc)
  (cond
    ((null l) acc)
    ((atom (car l)) (cond
		      ((member? (car l) acc) (f* (cdr l) acc))
		      (t(f* (cdr l) (cons (car l) acc)))))
    (t(f* (car l) (f* (cdr l) acc)))))

(f* lat2 '()) ;(fried baked)
(f* '(1 2 3 4 5 6) '()) ;(6 5 4 3 2 1)
(f* '(1 2 3 4 1 5 6 3 5 7) nil) ;(7 6 5 4 3 2 1)

;search for a in lat using accumulator acc
(defun occur** (a lat acc)
  (cond
    ((null lat) acc)
    ((eq (car lat) a) (occur** a (cdr lat) (+ 1 acc)))
    (t(occur** a (cdr lat) acc))))

(occur** 1 '(1 2 3 4 1 5 6 3 5 7) 0) ;2
(occur** 9 '(1 2 3 4 4 3 2 1) 0) ;0
     
  
