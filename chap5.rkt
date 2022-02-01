#lang racket
(require "primitive.rkt")
(require test-engine/racket-tests)
(provide (all-defined-out))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))
         ))
      (else (cons (rember* a (car l)) (rember* a (cdr l))))
      )
    )
  )

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
      )
    )
  )

(define add1
  (lambda (x)
    (+ x 1)
    )
  )

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l))))
      )
    )
  )
            

(check-expect (rember* 'a '(a b c d a b a)) '(b c d b))
(check-expect (rember* 'a '(a (b a d) c d a b a)) '((b d) c d b))
(check-expect (insertR* 'z 'a '(a b (b a c) d)) '(a z b (b a z c) d))
(check-expect (occur* 'a '(a b c d)) 1)
(test)