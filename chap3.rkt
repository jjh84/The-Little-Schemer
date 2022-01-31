#lang racket
(require "chap2.rkt")
(require test-engine/racket-tests)
(provide (all-defined-out))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat))))
      )
    )
  )

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l))))
      )
    )
  )

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat))))
      )
    )
  )

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertR new old (cdr lat))))
      )
    )
  )

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (subst new old (cdr lat)))
      )
    )
  )

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? o1 (car lat)) (cons new (cdr lat)))
      ((eq? o2 (car lat)) (cons new (cdr lat)))
      (else (subst2 new o1 o2 (cdr lat)))
      )
    )
  )

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat))))
      )
    )
  )

(check-expect (rember 'a '(a b c)) '(b c))
(check-expect (rember 'z '(a b c)) '(a b c))
(check-expect (firsts '((a b) (c d) (e f))) '(a c e))
(check-expect (insertR 'z 'a '(a b c)) '(a z b c))
(check-expect (insertL 'z 'a '(a b c)) '(z a b c))
(check-expect (subst 'z 'a '(a b c)) '(z b c))
(check-expect (subst2 'z 'a 'b '(a c e)) '(z c e))
(check-expect (subst2 'z 'a 'b '(b c e)) '(z c e))
(check-expect (multirember 'a '(b a c a)) '(b c))


(test)