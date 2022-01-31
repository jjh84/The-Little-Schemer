#lang racket

(require "basic.rkt")
(require test-engine/racket-tests)
(provide (all-defined-out))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat)))
      )
    )
  )

(check-expect (lat? '(a b c)) #t)
(check-expect (member? 'a '(a b c)) #t)
(check-expect (member? 'a '(b c d)) #f)
(check-expect (member? 'a '()) #f)
(test)