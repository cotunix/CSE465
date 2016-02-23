; Copyright Kyle Richardson 2016, Adapted from code provided by Dr. Michael Zmuda

; zipcodes.scm contains all the US zipcodes.
; You should not modify this file. Your code
; should work for other instances of this file.
(load "zipcodes.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
	#t
)

; Returns the roots of the quadratic formula, given
; ax^2+bx+c=0

(define (quadratic a b c)
	; Helper function purely for readability
	(define (quadrt a b c)
		(sqrt (- (* b b) (* (* 4 a) c)))
	)
        (list (/ (+ (- b) (quadrt a b c)) (* 2 a))
              (/ (- (- b) (quadrt a b c)) (* 2 a)))

)

(mydisplay (quadratic 1 0 0))
(mydisplay (quadratic 0 1 0))
(mydisplay (quadratic 3 4 2))

; Return a list with the items in reverse order
(define (reverse lst)
	(cond
		((= (length lst) 1) (list (car lst) ))
		((LIST? lst) (append (reverse (cdr lst)) (list (car lst))))
	)
)

(mydisplay (reverse '(1 2 3 4)))

; Returns a list that is identical to lst, but with all
; instances of v1 replaced with v2.
; (replace '(a b c (c a b) b a) 'a 'b) -> (b b c (c b b) b b)
; lst -- list of items, possibly nested.
; v1 & v2 -- atoms
; 565 students only
(define (replace lst v1 v2)
	lst
)

(mydisplay (replace '(a b c c b a) 'a 'b))
(mydisplay (replace '(a b c (c a b) b a) 'a 'b))

; Returns a list of two numeric values. The first is the smallest
; in the list and the second is the largest in the list. 
; lst -- flat, contains numeric values, and length is >= 1.
(define (minAndMax lst)
	(define (minimum lst num)
		(cond
			((= (length lst) 0) num)
			((< (car lst) num) (minimum (cdr lst) (car lst)))
			(else (minimum (cdr lst) num))
		)
	)
	(define (maximum lst num)
		(cond
			((= (length lst) 0) num)
			((> (car lst) num) (maximum (cdr lst) (car lst)))
			(else (maximum (cdr lst) num))
		)
	)
	(list (minimum (cdr lst) (car lst)) (maximum (cdr lst) (car lst)))
	
)


; Returns a list of three numbers (numNeg numZero numPos),
; where these numbers correspond to the number of negative
; numbers, number of zeros, and the number of positive numbers.
; For example (posneg '(-9 2 3 0 -2 -8 0)) should return
; (3 2 2). Approximately, 25% of this problem's points will be
; awarded for doing this with just one pass through the list.
; lst -- flat list containing numeric values, and length is >= 1.
(define (posneg lst)
	(define (posneghelp lst neg zer pos)
		(cond
			((= (length lst) 0) (list neg zer pos))
			((< (car lst) 0) (posneghelp (cdr lst) (+ neg 1) zer pos))
			((> (car lst) 0) (posneghelp (cdr lst) neg zer (+ pos 1)))
			((= (car lst) 0) (posneghelp (cdr lst) neg (+ zer 1) pos))
		)
	)
	(posneghelp lst 0 0 0)
)

(mydisplay (posneg '(1 2 3 4 2 0 -2 3 23 -3)))
(mydisplay (posneg '(-1 2 -3 4 2 0 -2 3 -23 -3 0 0)))
(mydisplay (posneg '()))

; The paramters are two flat lists with the same length.
; The inputs '(1 2 3) and '(a b c) should return a single list:
; ((1 a) (2 b) (3 c))
; lst1 & lst2 -- two flat lists with same length.
(define (zip lst1 lst2)
	(cond 
		((= (length lst1) 1) (list(list (car lst1) (car lst2))))
		(else (cons (list (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))) 
		
	) 
)

(mydisplay (zip '("Smith" "Jackson" "Wilson") '(35 28 21)))

; Returns all the information for a particular zip code
; zipcode -- 5 digit integer
; zips -- the zipcode DB
(define (getZipcodeInfo zipcode zips)
	(cond
		((= (caar zips) zipcode) (car zips))
		(else (getZipcodeInfo zipcode (cdr zips)))
	)
)

; Returns a list of all the states that contain the given place.
; The list of states should list the relevant states only once.
; placeName -- is the text corresponding to the name of the place
; zips -- the zipcode DB
(define (getStatesThatContainThisPlace placeName zips)
	(define (statesHelp placeName zips current)
		(cond 
			((= (length zips) 0) '())
			((string=? (caddar zips) current) (statesHelp placeName (cdr zips) current))
			((string=? (cadar zips) placeName) (cons (caddar zips) (statesHelp placeName (cdr zips) (caddar zips))))
			(else (getStatesThatContainThisPlace placeName (cdr zips)))
		)
	
	)
	(statesHelp placeName zips "")
)


; Returns the state that contains the most unique zip codes
; Return the first state if there is a tie
; zips -- zipcode DB
(define (getStateWithMostZipcodes zips)
	(define (mostHelp zips mx mxs cur curs)
		(cond
			((= (length zips) 0) mxs)
			((string=? (caddar zips) curs) (mostHelp (cdr zips) mx mxs (+ cur 1) curs))
			((and (not (string=? (caddar zips) curs)) (> cur mx)) (mostHelp (cdr zips) cur curs 1 (caddar zips)))
			(else (mostHelp (cdr zips) mx mxs 1 (caddar zips)))
		)
	)
	(mostHelp zips 0 "" 0 "")
)

; Returns the distance between two zip codes.
; Use lat/lon. Do some research to compute this.
; zip2 -- zipcode DB
; zip1 & zip2 -- the two zip codes in question.
; 565 students only
(define (getDistanceBetweenZipCodes zips zip1 zip2)
	0
)
(mydisplay (getZipcodeInfo 45056 zipcodes))
(mydisplay (getStatesThatContainThisPlace "Oxford" zipcodes))
(mydisplay (getStateWithMostZipcodes zipcodes))
(mydisplay (getDistanceBetweenZipCodes zipcodes 45056 48122))


; Some sample predicates
(define (POS? x) (> x 0))
(define (NEG? x) (> x 0))
(define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (NOT (LARGE? x)))

; Returns a list of items that satisfy a set of predicates.
; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
; (filterList '(1 2 3 4 100) '(EVEN? SMALL?))  rhturn should(2 4)
; lst -- flat list of items
; filters -- list of predicates to apply to the individual elements
(define (filterList lst filters)
	(define (filterhelp lst filt)
		(cond
			((= (length lst) 0) '())
			(((eval filt (interaction-environment)) (car lst)) (cons (car lst) (filterhelp (cdr lst) filt)))
			(else (filterhelp (cdr lst) filt))
		)
	)
	(cond
		((not(= (length filters) 0)) (filterList (filterhelp lst (car filters)) (cdr filters)))
		(else lst)
	)

		


)

(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN? LARGE?)))
