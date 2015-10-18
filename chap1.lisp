;@book: The Little Lisper
;@author: Gangadhar Nittala
;Chapter#1 and Chapter#1 Exercises
;Chapter#1 - Toys


; this is a convenience for using a variable
; a single quote means a literal value - i.e. letting lisp
; know that this shouldn't be evaluated

;; ===============
;; The law of car
;; car is defined only for non-null lists
;; ===============

(defparameter l1 '(((hotdogs)) (and) (pickle) relish))
(defparameter l2 '(a b c))
(defparameter l3 '((a b c) x y z))
(defparameter l4 '((x) t r))
(car l1)
(car (car l1))
(cdr l2)
(cdr l3)
(cdr l4)
;error
;(cdr 'hotdogs)
(cdr ())

;; ===============
;; The Law of cdr
;; cdr is defined only for non-null lists
;; The cdr of any non-null list is always another list
;; ===============

(defparameter l5 '( (b) (x y) ((c))) )
(defparameter l6 '( a (b (c)) d))
(defparameter a1 'peanut)
(defparameter l7 '(butter and jelly))
(defparameter l8 '(mayonnaise and))
(defparameter l9 '(peanut butter and jelly))
(defparameter l10 '(a b (c)))
(car (cdr l5))
(cdr (cdr l5))
; This will not compile - since (car l6)
; is an atom and we can't cdr an atom
;(cdr (car l6))
(cons a1 l7)
(cons l8 l9)
(cons l10 ())
(cons 'a ())
; this will evaluate successfully
; the result will be ((A B (C)) . B)
; Even though the second argument of cons 
; has to be a list, an atom is converted to
; a list and then the first argument is cons-ed
; to that list. This does violate the law of cons (below)
; Avoid this where possible
(cons l10 'b)
; just as the above, this will also evalute
; result will be (A . B)
(cons 'a 'b)

;; ===============
;; The law of cons
;; Cons takes two arguments
;; The second argument must be a list
;; The result is a list
;; ===============

(defparameter l11 '((b) c d))
(cons 'a (car l11))
(cons 'a (cdr l11))
(null ())
(null '())
(null '(a b c))
; this is an error, since spaghetti is not defined
; (null spaghetti)

;; ===========
;; The law of Null?
;; Null? is defined only for lists
;; ===========
(defparameter l12 '(Harry had a heap of apples))
(defparameter l13 '(swing low sweet cherry))
(defparameter l14 '(swing (low sweet) cherry))
(atom 'Harry)
(atom l12)
(atom (car l12))
(atom (cdr l12))
; note that () is an atom too
(atom (cdr '(Harry)))
(atom (car (cdr l13)))
(atom (car (cdr l14)))
(eq 'Harry 'Harry)
(eq 'margarine 'butter)
(eq '() '(strawberry))
;; ============
;; The law of Eq?
;; Eq? takes two arguments
;; Each must be an atom
;; ============
(eq (car '(Mary had a little lamb chop)) 'Mary)
;result is Nil, since the cdr returns (milk) and that is <> milk
(eq (cdr '(soured milk)) 'milk)
(defparameter l15 '(beans beans we need jelly beans))
(eq (car l15) (car (cdr l15)))

;implementation note: using defparameter instead of defvar
;http://stackoverflow.com/a/8928038
;http://lispsamik.blogspot.in/2007/09/defvar-and-defparameter.html

;; ===============
;; Exercises
;; ===============
(defparameter a 'all)
(defparameter b 'these)
(defparameter c 'problems)
(defparameter d '())

;(all (these problems))
(cons a (cons (cons b (cons c d)) d))
;(all (these) problems)
(cons a (cons (cons b d) (cons c d)))
;((all these) problems)
(cons (cons a (cons b d)) (cons c d))
;((all these problems))
(cons (cons a (cons b (cons c d))) d)

(defparameter a 'french)
(defparameter l '(fries))
(car (cons a l)) ;french

(defparameter a 'oranges)
(defparameter l '(apples and peaches))
(cdr (cons a l))

(defparameter y 'lisp)
(defparameter x 'lisp)
(eq x y)

(defparameter a 'atom)
(null (cons a '())) ;there isn't a list that makes this true

(cons 'x 'y) ; (x.y)
(cons '() '()) ; (nil)
(car '()) ;nil
(cdr '(())) ;nil

(atom (car '((meatballs) and spaghetti))) ;false, since car gives (meatballs) and that isn't an atom
(null (cdr '((meatballs)))) ;true, since cdr is ()
(eq (car '(two meatballs)) (car (cdr '(two meatballs)))) ;nil
(atom (cons 'meat '(ball))) ;nil, since (meat ball) is a list and not atom

(car (cdr (cdr (car '((kiwis mangoes lemons) and (more)))))) ;lemons
(car (cdr (car (cdr '(() (eggs and (bacon)) (for) (breakfast)))))) ;and
(car (cdr (cdr (cdr '(() () () (and (coffee)) please))))) ; (and (coffee))

(car (cdr (cdr '(peanut butter and jelly on toast)))) ;and
(car (car (cdr (cdr '(apples in (Harry has a backyard)))))) ;Harry
(car (cdr (cdr '(apples and Harry)))) ;Harry
(car (car (car (cdr (cdr (car '(((apples) and ((Harry))) in his backyard))))))) ;Harry


