;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#2 and Chapter#2 Exercises
;Chapter#2 - Do it, Do it Again, and Again, and Again..

(defparameter l1 '(Jack Sprat could eat no chicken fat))
(defparameter l2 '((Jack) Sprat could eat no chicken fat))
(defparameter l3 '(Jack (Sprat could) eat no chicken fat))

(defun lat? (l)
      (cond
	( (null l) t)
	( (atom (car l)) (lat? (cdr l)))
	( t nil)))

(lat? l1) ; T
(lat? l2) ; NIL
(lat? l3) ; NIL

(or (null ()) (atom '(d e fg))) ; T
(or (null '(a b c)) (null ())) ; T
(or (null '(a b c)) (null '(atom))) ; NIL


(defun member1? (a lat)
  (cond
    ((null lat) nil)
    ((eq a (car lat)) t)
    (t (member1? a (cdr lat)))))

(defun member2? (a lat)
  (cond
    ((null lat) nil)
    (t (or
	(eq (car lat) a)
	(member2? a (cdr lat))))))

;; ===============
;; The first commandment
;; Always ask null as the first question
;; in expressing any function
;; ===============

;; ===============
;; Exercises
;; ===============

(defparameter l1 '(german chocolate cake))
(defparameter l2 '(poppy seed cake))
(defparameter l3 '((linzer) (tote) ()))
(defparameter l4 '((bleu cheese) (and) (red) (wine)))
(defparameter l5 '(() ()))
(defparameter a1 'coffee)
(defparameter a2 'seed)
(defparameter a3 'poppy)

(lat? l1) ; T
(lat? l2) ; T
(lat? l3) ; NIL - has lists
(member2? a1 l1) ; NIL
(member2? a2 l2) ; T

;lat using if
(defun lat2? (l)
  (if (null l)
      t
      (if (atom (car l))
	  (lat2? (cdr l))
	  nil)))

(lat2? l1) ; T
(lat2? l2) ; T
(lat2? l3) ; NIL

;member using if
(defun member3? (a lat)
  (if (null lat)
      nil
      (if (eq a (car lat))
	  t
	  (member3? a (cdr lat)))))

(member3? a1 l1) ; NIL
(member3? a2 l2) ; T

(defun nonlat? (l)
  (cond
    ((null l) t)
    ((lat? l) nil)
    (t t)))


(nonlat? l1) ; NIL
(nonlat? l2) ; NIL
(nonlat? l3) ; T. The book seems to have an error here, since l3 is non-lat
(nonlat? l4) ; T

(defun member-cake? (l)
  (member1? 'cake l))

(member-cake? l1) ; T
(member-cake? l2) ; T
(member-cake? l3) ; NIL

(defun member4? (a lat)
  (cond
    ((null lat) nil)
    (t (or
	(member4? a (cdr lat))
	(eq a (car lat))))))

(member3? a1 l1) ; NIL
(member4? a1 l1) ; NIL
(member3? a2 l2) ; T
(member4? a2 l2) ; T
;Yes they both return the same values

(member3? a2 l3) ;NIL
(member4? a2 l3) ;NIL

;with the current known functions, this one doesn't seem to be possible
; using and to get this done
(defun member-twice? (a lat)
  (cond
    ((null lat) nil)
    ((and (eq a (car lat))
	  (member1? a (cdr lat)))) ;use member1? to check cdr and return bool
    (t (member-twice? a (cdr lat))))) ;if the car <> a, then check cdr

(defun member-twice2? (a lat)
  (cond
    ((null lat) nil)
    ((member1? a lat) (member1? a (cdr lat)))
    (t nil)))
	
(member-twice? 'german (cons 'german l1)) 
(member-twice? 'german '(this is a lat with german twice and german second))
(member3? a1 l1) ; NIL
(member4? a1 l1) ; NIL
(member3? a2 l2) ; T
(member4? a2 l2) ; T
;Yes they both return the same values

(member3? a2 l3) ;NIL
(member4? a2 l3) ;NIL

;with the current known functions, this one doesn't seem to be possible
; using and to get this done
(defun member-twice? (a lat)
  (cond
    ((null lat) nil)
    ((and (eq a (car lat))
	  (member1? a (cdr lat)))) ;use member1? to check cdr and return bool
    (t (member-twice? a (cdr lat))))) ;if the car <> a, then check cdr

;this one will not work for the case where the last 
;atom is the member - check the test case below with cake
(defun member-twice2? (a lat)
  (cond
    ((null lat) nil)
    ((member1? a lat) (member1? a (cdr lat)))
    (t nil)))
	
(member-twice? 'german (cons 'german l1))  ; T
(member-twice? 'german '(this is a german string with german twice)) ; T
(member-twice2? 'german '(this is a german string with german twice)) ; T

(member-twice? 'cake  l2) ;valid
(member-twice2? 'cake l2) ;invalid

(member-twice? 'cake '(cake at the beginning and cake in the middle)) ; valid
(member-twice2? 'cake '(cake at the beginning and cake in the middle)) ;valid

(member-twice? 'cake '(cake in the beginning  but at the end no cake)) ; T
(member-twice2? 'cake '(cake in the beginning but at the end no cake)) ; T

(member-twice?  'cake '(and he said cake))  ; valid
(member-twice2? 'cake '(and he said cake))  ; invalid
