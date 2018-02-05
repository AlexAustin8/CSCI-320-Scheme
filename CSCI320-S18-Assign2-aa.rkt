#lang racket
;Alexander Austin
;CSCI 230
;Dr. Pothering
;31 January 2018
;Assignment 2


;1. Define a function permutation? that takes two lists as parameters
;   and tests to see if they are permutations of each other


(define (permutation? lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) #t)
    ((eqv? (eqlength? lst1 lst2) #f) #f)
    (else (and (aux_permutation lst1 lst2) (aux_permutation lst2 lst1)))
    )
  )

(define (aux_permutation lst1 lst2)
  (cond
    ((and (null? (cdr lst1)) (member? (car lst1) lst2)) #t)
    ((eqv? (member? (car lst1) lst2) #f) #f)
    (else (aux_permutation (cdr lst1) lst2))
    )
  )

(define (eqlength? lst1 lst2)
  (cond
    ((and (eqv? (null? lst1) (null? lst2)) (eqv? (null? lst1) #t)) #t)
    ((eqv? (eqv? (null? lst1) (null? lst2)) #f) #f)
    (else (eqlength? (cdr lst1) (cdr lst2)))
    )
  )

(define(member? atm lst)
     (cond
         ((null? lst) #f)
         ((eqv? atm (car lst)) #t)
         (else (member? atm(cdr lst)))
     )
  )

(define (removefirst atm lst)
  (cond
    ((null? lst) '())
    ((eqv? atm (car lst)) (removefirst atm (cdr lst)) (cons (car lst) (removefirst atm (cdr lst))))
     )
  )

;2.a: Define a Function tree? that accepts a list as an argument and returns
;     #t if it is a tree and #f otherwise

(define (tree? lst)
  (cond
    ((null? lst) #t)
    ((and (atom? (car lst)) (list? (car(cdr lst))) (car(cdr(cdr lst)))) (and(tree? (cadr lst)) (tree? (caddr lst))))
    (else #f)
  )
  )
  

(define (atom? x)
  (and(not(pair? x)) (not (null? x)))
  )

   

;2.b Define a function preorder that accepts a tree as a parameter and returns a list containing the node values from a pre-order traversal
(define (preorder tree)
  (cond
    ((not (tree? tree)) '())
    ((null? tree) '())
    (else (append (append(list(car tree)) (preorder (cadr tree))) (preorder (caddr tree))))
          )
    )
  
  


(define(append list1 list2)
     (cond
        ((null? list1) list2)
        ((null? list) list1)
        (else(cons(car list1)(append (cdr list1) list2)))
        )
  )


;2.c Define a function inorder that accepts a tree as a parameter and returns a list containing the node values from a in-order traversal
(define (inorder tree)
  (cond
    ((not (tree? tree)) '())
    ((null? tree) '())
    (else (append (append (inorder (cadr tree)) (list(car tree))) (inorder (caddr tree))))
          )
    )


;2.d Define a function postorder that accepts a tree as a parameter and returns a list containing the node values from a post-order traversal
(define (postorder tree)
  (cond
    ((not (tree? tree)) '())
    ((null? tree) '())
    (else (append (append (postorder (cadr tree)) (postorder (caddr tree))) (list(car tree)) ))
          )
    )

;Problem 3: Create a function simplify_poly that takes a polynomial expression
;and simplifies it by removing extraneous information

;Here are the polynomial definitions as written by Dr. Pothering

; this predicate just tells us whether a term is a constant term
(define constant? number?)

; this predicate tells us whether a term is a variable named x
(define (variable? T x) 
  (and (symbol? T) (eqv? T x)))

; this predicate tells us whether we have a term of the form (k x) representing the polynomial kx
(define (linear_term? T x)
  (cond ((and (number? (car T)) (variable? (cadr T) x) (null? (cddr T))))
        (else #f)))

; this predicate defines a "term in x" as either a constant, the variable x, a linear term in x
;  or a term of the form (a x n), where a and n are numbers.
(define (term? T x) 
  (cond ((constant? T) #t)
        ((variable? T x) #t)
        ((linear_term? T x) #t)
        ((and (number? (car T)) (variable? (cadr T) x) (number? (caddr T)) (null? (cdddr T))))
        (else #f)))

; this predicate tells us whether a given expression is a list of "terms in x"
(define (term_list? LT x)
   (cond ((null? LT) #t)
         ((and (term? (car LT) x) (term_list? (cdr LT) x)) #t)
         (else #f)))


; this predicate defines a polynomial E in x as having the form of a term,
;  or the form (+ term_in_x term_in_x...term_in_x)
;  that is a list whose first element is the plus sign and whose cdr is a list of terms in x
(define (polynomial? T x)
  (cond ((term? T x) #t)
        ((and (eqv? (car T) '+) (term_list? (cdr T) x)) #t)
        (else #f)))

; here we define some more meaningfully named functions for handling terms in x of the form (a x n)
(define (get-coefficient T) (car T))
(define (get-variable T) (cadr T))
(define (get-exponent T) (caddr T))


; now we implement symbolic derivatives of terms.  Note, the derivative of a term in x with respsect to a variable
;  other than x is 0.  Likewise, the derivative with respect to x of a term in a variable other than x is 0.

(define (d-term T x)
  (cond ((constant? T) 0)
        ((variable? T x) 1)
        ((linear_term? T x) (get-coefficient T))
        ((term? T x) (list (* (get-coefficient T) (get-exponent T))
                         (get-variable T)
                         (- (get-exponent T) 1)))
        (else 0)))

; the derivative of a list with respect to x of a list of terms will be the list of derivatives
(define (d-term_list TL x)
  (cond ((null? TL) '())
        (else (cons(d-term (car TL) x) (d-term_list (cdr TL) x))))) 

; a polynomial is a sum of terms in x, so the derivative of a polynomial is the sum of the derivatives
;  of its constituent terms with respct to x
(define (d-polynomial P x)
  (cond ((term? P x) (d-term P x))
        ((polynomial? P x) (cons (car P)  (d-term_list (cdr P) x)))
        (else 0)))

;End polynomial definitions




(define(simplify_poly lst)
  (cond
  ((not (term_list? (cdr lst) 'x)) '())
  ((and (variable? (cadr lst) 'x) (eqv? (caddr lst) 0)) (cadr lst))
  ((and (variable? (caddr lst) 'x) (eqv? (cadr lst) 0)) (caddr lst))
  (else (append (list (car lst)) (simplify_poly_aux '() (cdr lst))))
  )
  )

(define(simplify_poly_aux newlst oldlst)
  (cond
    ((null? oldlst) newlst)
    ((and (number? (car oldlst)) (zero? (car oldlst))) newlst)
    ((number? (car oldlst)) (simplify_poly_aux(append newlst (list (car oldlst))) (cdr oldlst)))
    ((and (term? (car oldlst) 'x) (eqv? (car(car oldlst)) 0)) (simplify_poly_aux newlst (cdr oldlst)))
    ((and (term? (car oldlst) 'x) (eqv? (caddr(car oldlst)) 0)) (simplify_poly_aux (append newlst (list (car(car oldlst)) (cadr(car oldlst)))) (cdr oldlst)))
    ((and (term? (car oldlst) 'x) (eqv? (car(car oldlst)) 1)) (simplify_poly_aux (append newlst (list (cadr(car oldlst)) caddr(car oldlst)))  (cdr oldlst)))
    ((and (term? (car oldlst) 'x) (eqv? (caddr(car oldlst)) 1)) (simplify_poly_aux (append newlst (list (list (car(car oldlst)) (cadr(car oldlst))))) (cdr oldlst)))
    
    (else (simplify_poly_aux (append newlst (list(car oldlst))) (cdr oldlst)))
    )
  )

;Problem 4.a: Define a function product? that takes an expression E as a parameter
;and return #t if the expression represents the product of two polynomials
;and #f otherwise

(define (product? lst)
  (cond
    ((null? lst) #f)
    ((and (eqv? (car lst) '*) (term_list? (cdr lst) 'x)) #t)
    (else #f)
    )
  )

;Problem 4.b: Define a function d_product that takes an expression E and a variable
;x as parameters and returns the derivative of E in x

(define (d_product E x)
  (cond
    ((null? E) '())
    ((and (product? E) (term_list? (cdr E) x))
       (append '(+)
               (list(append (append '(*) (append (append '() (list (cadr E))) (list (d-term (caddr E) 'x))))
               (append '(*) (append (append '() (list (caddr E))) (list (d-term (cadr E) 'x))))
               )))
       )
    (else '())
    )
  )



  
    
  
    

