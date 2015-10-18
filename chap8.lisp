;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#8 and Chapter#8 Exercises
;Chapter#8 - Friends and Relations


;member* is from chap7.lisp
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

;is lat a set
(defun set? (lat)
  (cond
    ((null lat) t)
    ((member* (car lat) (cdr lat)) nil)
    (t (set? (cdr lat)))))

(set? '(apple peaches apple plums)) ;nil
(set? '(apple peaches pears plums)) ;T
(set? '(apple 3 pear 4 9 apple 3 4)) ;nil
(set? '(apple 3 pear 4 9 pears 5 plums)) ;T

;make a set out of lat
(defun makeset (lat)
  (cond
    ((null lat) nil)
    ((member* (car lat) (cdr lat)) (makeset (cdr lat))) ;if car exists, ignore it and recurse on cdr
    (t (cons  ; doesn't exist any more, so use it to build list
	(car lat) 
	(makeset (cdr lat))))))

(makeset '(apple peach pear peach plum apple lemon peach)) ;(pear plum apple lemon peach)

;from chap5.lisp - removes multiple occurences of a in lat
(defun multirember (a lat)
  (cond ((null lat) nil)
	((eq (car lat) a) (multirember a (cdr lat)))
	(t(cons (car lat) (multirember a (cdr lat))))))

;does the same thing as makeset earlier
;The difference is that this retains the order of occurence when removing items
(defun makeset (lat)
  (cond ((null lat) nil)
	(t (cons
	    (car lat) ;take car
	    (makeset (multirember (car lat) (cdr lat))))))) ;cons it with the list removing it from the lat and recursing
(makeset '(apple peach pear peach plum apple lemon peach)) ;(apple peach pear plum lemon)
(makeset '(apple 3 pear 4 9 apple 3 4)) ;(apple 3 pear 4 9)

;are all elements in set1 in set2
;assumption is that the inputs are sets
(defun subset? (set1 set2)
  (cond
    ((null set1) t)
    (t (and
	(member* (car set1) set2)
	(subset? (cdr set1) set2)))))
;The book sticks to the commandments and writes the above as follows
(defun subset-book? (set1 set2)
  (cond
    ((null set1) t)
    (t(cond
	((member* (car set1) set2) (subset-book? (cdr set1) set2))
	(t nil)))))

(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) ;T
(subset? '(4 pounds of horseradish) '(four pinds chicken and 5 ounces horseradish)) ;nil
(subset-book? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) ;T
(subset-book? '(4 pounds of horseradish) '(four pinds chicken and 5 ounces horseradish)) ;nil

;if set1 subset set2 and set2 subset of set1, then they both are equal
(defun eqset? (set1 set2)
  (cond
    ((subset? set1 set2) (subset? set2 set1))
    (t nil)))

(eqset? '(6 large chickens with wings) '(6 large chickens with wings)) ;T

;if there is atleast one element in set1 in set2, return true
(defun intersect? (set1 set2)
  (cond
    ((null set1) t)
    ((member* (car set1) set2) t)
    (t (intersect? (cdr set1) set2))))

(intersect? '(tomatoes and macaroni) '(macaroni and cheese)) ;T

(defun intersect-or? (set1 set2)
  (cond
    ((null set1) t)
    (t (or
	(member* (car set1) set2)
	(intersect-or? (cdr set1) set2)))))

(intersect-or? '(tomatoes and macaroni) '(macaroni and cheese)) ;T

;return the intersection between set1 and set2
(defun intersect (set1 set2)
  (cond
    ((null set1) nil)
    ((member* (car set1) set2) (cons 
				(car set1)
				(intersect (cdr set1) set2)))
    (t(intersect (cdr set1) set2))))

(intersect '(tomatoes and macaroni) '(macaroni and cheese)) ;(and macaroni)

(defun intersect-not (set1 set2)
  (cond
    ((null set1) nil)
    ((not (member* (car set1) set2)) (intersect-not (cdr set1) set2))
    (t (cons (car set1) (member* (cdr set1) set2)))))

;create the union of two sets - duplicates are returned once
(defun union-1 (set1 set2)
  (cond
    ((null set1) set2)
    ((not (member* (car set1) set2)) (cons
				      (car set1)
				      (union-1 (cdr set1) set2)))
    (t(union-1 (cdr set1) set2))))

(union-1 '(tomatoes and macaroni casserole) '(macaroni and cheese)) ;'(tomatoes casserole macaroni and cheese)

;return members in set1 that are not in set2
(defun xxx (set1 set2)
  (cond
    ((null set1) nil)
    ((member* (car set1) set2) (xxx (cdr set1) set2))
    (t (cons (car set1)
	     (xxx (cdr set1) set2)))))
(xxx '(tomatoes and macaroni casserole) '(macaroni and cheese)) ;'(tomatoes casserole)

(defun intersectall (l-set)
  (cond
    ((null l-set) nil)
    ((null (cdr l-set)) (car l-set))
    (t
     (intersect (car l-set)
		(intersectall (cdr l-set))))))

(defparameter lset1 '((a b c) (c a d e) (e f g h a b)))
(intersectall lset1) ;(a)

(defparameter lset2 '((6 pears and) (3 peaches and 6 peppers)
		      (8 pears and 6 plums) (and 6 prunes with lots of apples)))
(intersectall lset2) ;(6 and)


(defun first-p (p)
  (car p))
(defun second-p (p)
  (car (cdr p)))
(defun build-p (a1 a2)
  (cons a1 (cons a2 '())))
(defun third-p (p)
  (car (cdr (cdr p))))


(defun fun-1? (rel)
  (cond
    ((null rel) t)
    ((member* (car rel) (cdr rel)) nil) ;car exits in cdr, so rel not unique
    (t (fun-1? (cdr rel)))))

(defun firsts-p (rel)
  (cond
    ((null rel) nil)
    (t(cons (car (first rel))
	    (firsts-p (cdr rel))))))
(firsts-p '((8 3) (4 2) (7 6) (6 2) (3 4))) ;(8 4 7 6 3)

(defun seconds-p (rel)
  (cond
    ((null rel) nil)
    (t(cons (second (car rel))
	    (seconds-p (cdr rel))))))
(seconds-p '((8 3) (4 2) (7 6) (6 2) (3 4))) ;(3 2 6 2 4)

(defun fun-2? (rel)
  (cond
    ((null rel) t)
    ((member* (first (car rel)) (firsts-p (cdr rel))) nil)
    (t (fun-2? (cdr rel)))))
(fun-2? '((8 3) (4 2) (7 6) (6 2) (3 4))) ;T
(fun-2? '((8 3) (4 2) (8 8))) ;nil - 8 is repeated

(defun fun? (rel)
  (set? (firsts-p rel)))

(defun revrel (rel)
  (cond
    ((null rel) nil)
    (t (cons
	(build-p
	 (second (car rel))
	 (first  (car rel)))
	(revrel (cdr rel))))))

(revrel '((8 a) (pumpkin pie) (got sick))) ;((a 8) (pie pumpkin) (sick got))

(defun fullfun? (rel)
  (set? (seconds-p rel)))

(fullfun? '((grape raisin) (plum prune) (stewed grape))) ;T
(fullfun? '((grape raisin) (plum prune) (stewed prune))) ;nil

(defun one-to-one? (rel)
  (fun? (revrel rel)))

;; ===============
;; Exercises
;; ===============

(defparameter r1 '((a b) (a a) (b b)))
(defparameter r2 '((c c)))
(defparameter r3 '((a c) (b c)))
(defparameter r4 '((a b) (b a)))
(defparameter f1 '((a 1) (b 2) (c 2) (d 1)))
(defparameter f2 '())
(defparameter f3 '((a 2) (b 1)))
(defparameter f4 '((1 $) (3 *)))
(defparameter d1 '(a b))
(defparameter d2 '(c d))
(defparameter x 'a)

;8.1.1
;ideally both the parameters of union-1 should be sets
;Instead of making 2 sets, we make the union with a duplicate (which is logically wrong)
;and then make a set out of it
;Real code should be (union-1 (makeset(firsts-p rel)) (makeset (seconds-p rel)))
(defun domset (rel)
  (cond
    ((null rel) nil)
    (t(makeset (union-1 (firsts-p rel) (seconds-p rel))))))

(domset r1) ;(B A)
(domset r2) ;(C)
(domset r3) ;(A B C)

;8.1.2
;s is of the form (a b) - return ((a a) (b b))
(defun idrel (s)
  (cond
    ((null s) nil)
    (t (cons
	(build-p
	 (car s)
	 (car s))
	(idrel (cdr s))))))

(idrel d1) ;((A A) (B B))
(idrel d2) ;((C C) (D D))
(idrel f2) ;nil

;Is tup a member of ltup?
;assumption: tup is for (a b) and ltup is of form ((x y) (p q))
(defun tuple-member? (tup ltup)
  (cond
    ((null ltup) nil)
    ((and (eq (first-p tup)  (first-p (car ltup))) ;are the first and second equal between tup and (car ltup)
	  (eq (second-p tup) (second-p (car ltup)))) t)
    (t(tuple-member? tup (cdr ltup)))))

;Are all elements of lneedle in lhaystack ?
;assumption: both lneedle and lhaystack are list of form ((x y) (p q))
(defun tuples-member? (lneedle lhaystack)
  (cond
    ((null lneedle) t) ;null exists in all haystacks
    ((null lhaystack) nil)
    (t (and  (tuple-member?  (car lneedle) lhaystack)
	     (tuples-member? (cdr lneedle) lhaystack)))))


;8.2
;A relation is reflexive if it contains all pairs of the form (d d) where
;d is a domain of disclosure for the relation (8.1.1)
;Learnt a new thing thanks to this function - whenever we need a variable, that
;can be done as a return value from the function
;What I mean is - in an imperative world, the (idrel(domset rel)) would be stored in
;a ltuple and we would search if every tuple in this is available in the rel
;In the functional world though - offload this to tuples-member? which will search 
;if lneedle is in lhaystack - and it does this by checking the base case with the
;natural recursion. And the base-case uses another function to do the check for one tuple
;in a list of tuples.
;The reason I had to resort to this is because it is not possible to iterate over the 
;(idrel (domset rel)) since the natural recursion for this would modify the list to search -
; we want that to remain as-is. That is another way to recognize that we need a separate
; function to do the natural recursion
;The downside of this is that we iterate over the rel n^2 times where n is the length of 
;the (idrel (domset rel))

(defun reflexive? (rel)
  (cond
    ((null rel) nil)
    ((tuples-member? (idrel (domset rel)) rel) t)
    (t nil)))

(reflexive? r1) ;T
(reflexive? r2) ;T
(reflexive? r3) ;nil

;is tset1 a subset of tset2?
(defun tuple-subset? (tset1 tset2)
  (cond
    ((null tset1) t)
    (t (and
	(tuple-member? (car tset1) tset2)
	(tuple-subset? (cdr tset1) tset2)))))

;are ltup1 and ltup2 equal?
(defun eq-tuples? (ltup1 ltup2)
  (cond
    ((tuple-subset? ltup1 ltup2) (tuple-subset? ltup2 ltup2))
    (t nil)))

;8.3.1
;rel is symmetric if revrel == rel
(defun symmetric? (rel)
  (cond
    ((null rel) t)
    (t (eq-tuples? (revrel rel) rel))))


(symmetric? r1) ;nil
(symmetric? r2) ;T
(symmetric? f2) ;T

;return intersection of ltup1 and ltup2
(defun tuple-intersect (ltup1 ltup2)
  (cond
    ((null ltup1) nil)
    ((tuple-member? (car ltup1) ltup2) (cons 
					(car ltup1)
					(tuple-intersect (cdr ltup1) ltup2)))
    (t(intersect (cdr ltup1) ltup2))))

;8.3.2
(defun antisymmetric? (rel)
  (cond
    ((null rel) nil)
    (t(tuple-subset? 
       (tuple-intersect rel (revrel rel))
       (idrel (domset rel))))))

(antisymmetric? r1) ;T
(antisymmetric? r2) ;T
(antisymmetric? r4) ;nil

;8.4
;f is a ltup; search for x in f and return the second-p of the
;found value
(defun fapply (f x)
  (cond
    ((null f) nil)
    ((eq (car (first-p f)) x) (second-p (first-p f)))
    (t (fapply (cdr f) x))))

(fapply f1 x) ;1
(fapply f2 x) ;nil
(fapply f3 x) ;2


;8.5
;TODO: This is not done yet. f is not being recursed on, only g is being recursed
;Need to build upon cdr f and also cdr g; only cdr g is happening now
(defun fcomp (f g)
  (cond
    ((null f) nil)
    ((null g) nil)
    ((eq (first-p  (first-p f))
	 (second-p (first-p g))) (cons (build-p (first-p  (first-p g))
						(second-p (first-p f)))
				       (fcomp f (cdr g))))
    (t(fcomp f (cdr g)))))


(fcomp f1 f4) ;nil
(fcomp f1 f3) ;nil
(fcomp f4 f1) ;((A $) (D $))
(fcomp f4 f3) ;((B $))

;8.6
(defun Rapply (rel x)
  (cond
    ((null rel) nil)
    ((eq (first-p (first-p rel)) x) (cons
				     (second-p (first-p rel))
				     (Rapply (cdr rel) x)))
    (t (Rapply (cdr rel) x))))

(Rapply f1 x) ;(1)
(Rapply r1 x) ;(B A)
(Rapply f2 x) ;nil

;8.7
(defun Rin (x set)
  (cond
    ((null set) nil)
    (t(cons (build-p x (first-p set))
	    (Rin x (cdr set))))))

(Rin x d1) ;((A A) (A B))
(Rin x d2) ;((A C) (A D))
(Rin x f2) ; nil


;8.8
(defun Rcomp (rel1 rel2)
  (cond
    ((null rel1) nil)
    (t (union-1
	(Rin
	 (first-p (car rel1))
	 (Rapply rel2 (second-p (car rel1))))
	(Rcomp (cdr rel1) rel2)))))

(Rcomp r1 r3) ;((A C) (A C) (B C))
(Rcomp r1 f1) ;((A 2) (A 1) (B 2))
(Rcomp r1 r1) ;((A B) (A B) (A A) (B B))

;8.9
;will be true, because (Rcomp rel rel) will have duplicates
(defun transitive? (rel)
 (tuple-subset? (Rcomp rel rel) rel))

(transitive? r1) ;T
(transitive? r3) ;T
(transitive? f1) ;T
;any relation that composes to a new relation (i.e no duplicates) will
;return nil i.e. is not transitive
;Here (Rcomp '((x y) (y z)) '((x y) (y z))) will return a new element (x z)
;And this is not part of the original rel
(transitive? '((x y) (y z))) ;nil

;8.10
(defun quasi-order? (rel)
  (and (reflexive? rel)
       (transitive? rel)))

(defun partial-order? (rel)
  (and (quasi-order? rel)
       (antisymmetric? rel)))

(defun equivalence? (rel)
  (and (quasi-order? rel)
       (symmetric? rel)))
