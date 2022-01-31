#lang racket

(require test-engine/racket-tests)
(provide (all-defined-out))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
    )
  )

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f)
      )
    )
  )



(check-expect (lat? '()) #t)
(check-expect (lat? '(a b c)) #t)
(check-expect (lat? '(a (b c) d)) #f)
(test)
