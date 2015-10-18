;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#5 and Chapter#5 Exercises
;Chapter#5 - The multichapter chapter

; check if atom a is part of lat
(defun member? (a lat)
  (cond
    ( (null lat) nil)
    (t (or
	(eq (car lat) a)
	(member? a (cdr lat))))))

;remove first occurence of atom a from lat
(defun rember (a lat)
  (cond
    ((null lat) nil)
    (t (cond
       ( (eq (car lat) a) (cdr lat))
       ( t (cons
	      (car lat)
	      (rember a (cdr lat))))))))


; remove all occurences of a from lat
; the key is in the first condition
; once we find that the a eq (car lat),
; keep searching the (cdr lat) for a
; In the rember, we returned (cdr lat)
; - didn't search any further
; And the beauty is that we cons atoms
; when they are not equal
(defun multirember-1 (a lat)
  (cond ((null lat) nil)
	((eq (car lat) a) (multirember-1 a (cdr lat)))
	(t(cons (car lat) (multirember-1 a (cdr lat))))))
  
(defun multirember (a lat)
  (cond
    ((null lat) nil)
    (t (cond
	 ((eq (car lat) a)
	    (multirember a (cdr lat)))
	 (t (cons
	      (car lat)
	      (multirember a (cdr lat))))))))

;insert to the right of old, as many times old occurs
(defun multiinsertR-1 (old new lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) old) (cons (car lat) (cons new (multiinsertR-1 old new (cdr lat)))))
    (t(cons (car lat) (multiinsertR-1 old new (cdr lat))))))


; insert new to the right of old in lat
; however many times old occurs in lat
; The key is to check the insertR :)
; In insertR once old is found, we return (cdr lat)
; In this function though, we continue searching
(defun multiinsertR(old new lat)
  (cond
    ( (null lat) nil)
    (t (cond
	 ( (eq (car lat) old)
	     (cons old
		   (cons new
			 (multiinsertR old new (cdr lat)))))
	 (t (cons (car lat)
		  (multiinsertR old new (cdr lat))))))))

(multiinsertR 'fish 'fried '(chips and fish or fish and fried))

;insert new to the left of all occurences of old in lat
(defun multiinsertL-1 (old new lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) old) (cons new (cons old (multiinsertL-1 old new (cdr lat)))))
    (t (cons (car lat) (multiinsertL-1 old new (cdr lat))))))

(multiinsertL-1 'fish 'fried '(chips and fish or fish and fried))

; this is the same way as multiinsertL
; instead of consing old to the rest of the list
; cons the new to the rest of the list
(defun multiinsertL (old new lat)
  (cond
    ((null lat) nil)
    (t (cond
	 ( (eq (car lat) old)
	     (cons
	         new
		 (cons old
		       (multiinsertL old new (cdr lat)))))
	 (t (cons (car lat)
		  (multiinsertL old new (cdr lat))))))))
  
;; ===============
;; The Sixth Commandment
;; Always change atleast one argument while recurring
;; The changing argument(s) should be tested in the
;; terminating condition(s) and it should be changed
;; to be closer to termination
;; For example,
;;   When using cdr, test termination with  null
;;   When using sub1, test termination with zero
;; ===============

(defun multisubst-1 (old new lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) old) (cons new (multisubst-1 old new (cdr lat))))
    (t(cons (car lat) (multisubst-1 old new (cdr lat))))))

(multisubst-1 '1 '100 '(1 2 3 4 5 1 3 5 7 1 1 2 3))

; substitute old with new across lat
(defun multisubst (old new lat)
  (cond
    ( (null lat) nil)
    (t (cond
	 ( (eq old (car lat))
	      (cons new
		    (multisubst old new (cdr lat))))
	 (t (cons (car lat)
		  (multisubst old new (cdr lat))))))))

;count the # of times a occurs in lat
(defun occur-1 (a lat)
  (cond
    ((null lat) 0)
    ((eq (car lat) a) (+ 1 (occur-1 a (cdr lat))))
    (t (occur-1 a (cdr lat)))))

(occur-1 '1 '(1 2 3 4 5 1 3 5 7 1 1 2 3))


; return the number of times a occurs in lat
(defun occur (a lat)
  (cond
    ( (null lat) 0)
    (t (cond
	 ( (eq a (car lat))
	     (1+ (occur a (cdr lat))))
	 (t (occur a (cdr lat)))))))

(defun one? (a)
  (zerop (1- a)))

(defun rempick-1 (n lat)
  (cond
    ((null lat) nil)
    ((one? n) (cdr lat))
    (t( cons (car lat) (rempick-1 (- n 1) (cdr lat))))))

(rempick-1 5 '(1 2 3 4 5 1 3 5 7 1 1 2 3))
(rempick-1 1 '(1 2 3 4 5 1 3 5 7 1 1 2 3))


; remove nth element from lat
; n is 1-indexed
(defun rempick (n lat)
  (cond
    ( (null lat) nil)
    (t (cond
	 ((one? n) (cdr lat))
	 (t (cons (car lat)
		  (rempick (1- n) (cdr lat))))))))

;; ===============
;; Exercises
;; ===============

(defparameter x 'comma)
(defparameter y 'dot)
(defparameter a 'kiwis)
(defparameter b 'plums)
(defparameter lat1 '(bananas kiwis))
(defparameter lat2 '(peaches apples bananas))
(defparameter lat3 '(kiwis pears plums bananas cherries))
(defparameter lat4 '(kiwis mangoes kiwis guavas kiwis))
(defparameter l1 '((curry) () (chicken) ()))
(defparameter l2 '((peaches) (and cream)))
(defparameter l3 '((plums) and (ice) and cream))
(defparameter l4 '())


(defun multisubst-kiwis (new lat)
  (multisubst 'kiwis new lat))

(multisubst-kiwis b lat1) ;(bananas plums)
(multisubst-kiwis y lat2) ;(peaches apples bananas)
(multisubst-kiwis y lat4) ;(dot mangoes dot guavas dot)
(multisubst-kiwis y l4) ;nil

;subst2 from chap3.lisp
(defun subst2 (o1 o2 new lat)
  (cond
    ((null lat) nil)
    ((or (eq (car lat) o1)
	 (eq (car lat) o2)) (cons new (cdr lat)))
    (t (cons (car lat) (subst2 o1 o2 new (cdr lat))))))

(subst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping))	 

;replace all occurences of either o1 or o2 in lat with new
(defun multisubst2 (o1 o2 new lat)
  (cond
    ((null lat) nil)
    ((or (eq (car lat) o1)
	 (eq (car lat) o2)) (cons new (multisubst2 o1 o2 new (cdr lat))))
    (t (cons (car lat) (multisubst2 o1 o2 new (cdr lat))))))

;note: the parameters in the book and the e-book are different, hence
;hence changing the order of calling
(multisubst2 a b x lat1) ;(bananas comma)
(multisubst2 a b y lat3) ;(dot pears dot bananas cherries)
(multisubst2 x y a lat1) ;(bananas kiwis)

;replace all atoms in lat with a list of the atom
(defun multidown (lat)
  (cond 
    ((null lat) nil)
    ((atom (car lat)) (cons (cons (car lat) '()) (multidown (cdr lat))))
    (t (multidown (cdr lat)))))

(multidown lat1) ;((bananas) (kiwis))
(multidown lat2) ;((peaches) (apples) (bananas))
(multidown l4) ;nil

;count the occurences of each element of lat1 in lat2
(defun occurN (lat1 lat2)
  (cond
    ((or (null lat1)
	(null lat2)) 0)
    (t (+ (occur  (car lat1) lat2)
	  (occurN (cdr lat1) lat2)))))
	
(occurN lat1 l4) ;0
(occurN lat1 lat2) ;1
(occurN lat1 lat3) ;2

;return the first atom in lat2 that is in both lat1 and lat2
;do this by checking if car(lat2) occurs in lat1 and if so, return car(lat2)
;else iterate over (cdr lat2) or return zero if either is null
(defun I (lat1 lat2)
  (cond
    ((or (null lat1)
	 (null lat2)) nil)
    ((>  (occur (car lat2) lat1) 0) (car lat2))
    (t (I lat1 (cdr lat2)))))

(I lat1 l4) ;nil
(I lat1 lat2) ;bananas
(I lat1 lat3) ;kiwis

;return all occurences of lat2 that occur in lat1
;twisted code alert !
;if there is a single occurence of lat2 in lat1, then cons that occurence with
;the resultant lists lat1, lat2 where this intersection is removed
;i.e. find the first occurence of lat2 in lat1, then remove this occurence in both 
;and search through the cdr of lat2
;And when there is no further occurences of lat2 in lat1, return nil
(defun multiI (lat1 lat2)
  (cond
    ((or (null lat1)
	 (null lat2)) nil)
    ((I lat1 lat2) (cons (I lat1 lat2) (multiI (multirember (I lat1 lat2) lat1) (multirember (I lat1 lat2) lat2))))
    (t nil)))

; lat1 = (bananas kiwis)
; lat2 = (peaches apples bananas)
; lat3 = (kiwis pears plums bananas cherries)
(multiI lat1 l4) ; nil
(multiI lat1 lat2) ;(bananas)
(multiI lat1 lat3) ;(kiwis bananas)

(defun badone? (n)
    (cond
      ((zerop (sub1 n)) t)
      (t nil)))
;violates the first commandment - first check should be zero
;violates the 6th commandment - the input should be reduced and then recurse

(defun badeq (n m)
  (cond
    ((zerop n) (zerop m))
    (t(badeq n (sub1 m)))))
;violates 6th commandment because the argument that is changing (m) is not the one
;that is being tested as part of the termination check

(defun count0 (vec)
  (cond
    ((null vec) 0)
    ((zerop (car vec)) (+ 1 (count0 (cdr vec))))
    (t (count0 (cdr vec)))))

(defun count0 (vec)
  (cond
    ((null vec) 0)
    (t (cond
	 ((zerop (car vec)) (+ 1 (count0 (cdr vec))))
	 (t (zerop (cdr vec)))))))
    
(count0 '(1 2 3 0 1 4 5 0 7 8 9 0))

;replace all lats of length 1 in llat with the atom and remove nil
;anything else, leave that lat as-is in the result
(defun multiup (llat)
  (cond
    ((null llat) nil)
    (t (cond
	 ((eq (length (car llat)) 0) (multiup (cdr llat))) ;empty list length == 0, so ignore
	 ((eq (length (car llat)) 1) (cons (car (car llat)) (multiup (cdr llat)))) ;length==1, so get the first atom
	 (t (cons (car llat) (multiup (cdr llat)))))))) ;cons as-is anything else in the cdr


(multiup l4) ;nil
(multiup l1) ;(curry chicken)
(multiup l2) ;(peaches (and cream))
