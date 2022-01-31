#lang racket
(require "basic.rkt")
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
            

(check-expect (rember* 'a '(a b c d a b a)) '(b c d b))
(check-expect (rember* 'a '(a (b a d) c d a b a)) '((b d) c d b))
(check-expect (insertR* 'z 'a '(a b (b a c) d)) '(a z b (b a z c) d))
(test)