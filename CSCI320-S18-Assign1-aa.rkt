#lang racket
;Problem 1: Define a function make_list_of_size that takes as parameters an integer n and an expression e, and returns a list of n copies of e.
(define (make_list_of_size n e)
     (cond
        ((eqv? n 0) e)
        (else (make_list_of_size2 (- n 1) (cons e '())))
        )
  )
(define (make_list_of_size2 n e)
     (cond
        ((eqv? n 0) e)
        (else (make_list_of_size2 (- n 1) (cons (car e) e) ))
        )
  )


;Problem 2: Define a function zeros that returns the number of zeros in a given simple list of numbers
(define (zeros list)
      (cond
        ((null? list) 0)
        (else(zeros2 list 0))
        )
  )

(define (zeros2 list count)
  (cond
    ((null? list) count)
        ((eqv? (car list) 0) (zeros2 (cdr list) (+ count 1)))
        (else (zeros2 (cdr list) count))
        )
  )

;Problem 3 Define a scheme function power that takes two numeric atoms a and b , where b is an integer and returns a raised to the b power

(define (power a b)
  (cond
    ((eqv? b 0) 1)
    ((eqv? a 0) 0)
    ((> b 0) (powerpos a b a))
    (else (powerpos (/ 1 a) (abs b) ( / 1 a)))
    )
  )
(define (powerpos a b sum)
  (cond
  ((eqv? b 1) sum)
  (else (powerpos a (- b 1) (* sum a)))
  )
 )

;Problem 4: Define a function remove that takes a list and an atom as parameters, and removes all top level instances of parameter from list
(define (remove lst atom)
  (cond
    ((null? lst) '())
    (else (reverse (remove2 lst '() atom)))
    )
  )

(define (remove2 lst newlst atom)
  (cond
  ((null? lst) newlst)
  ((eqv? (car lst) atom) (remove2 (cdr lst) newlst atom))
  (else (remove2 (cdr lst)  (cons (car lst) newlst) atom))
    )
  )

(define (reverse lst)
  (cond
    ((null? lst) lst)
    (else (reverse2 lst '()))
    )
  )

(define (reverse2 lst newlst)
  (cond
    ((null? lst) newlst)
    (else (reverse2 (cdr lst) (cons (car lst) newlst)))
    )
  )
  



;Problem 5: Define a function largest that returns the largest value in a given list

(define (largest lst)
  (cond
    ((null? lst) '())
    ((eqv? (list? lst) #f) 0)
    (else (largest2 lst (car lst)))
    )
  )

(define (largest2 lst max)
  (cond
    ((null? lst) max)
    ((> (car lst) max) (largest2 (cdr lst) (car lst)))
    (else (largest2 (cdr lst) max))
    )
)


    


        
