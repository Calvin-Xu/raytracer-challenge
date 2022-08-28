#lang typed/racket
(provide (except-out (all-defined-out) _stripe stripe))
(require "tuples.rkt")
(require "color.rkt")
(require "matrix.rkt")

(struct _pattern ([color-at : (-> Point Color)] [transformation : Matrix])
  #:prefab
  #:type-name Pattern)

(: pattern (->* ((U 'stripe 'gradient 'checker) (Listof Color)) (#:transformation Matrix) Pattern))
(define (pattern type
          colors
          #:transformation [transformation id-mat-4])
  (cond
    [(eq? type 'stripe) (stripe (cast colors (List Color Color)) transformation)]
    [else (error "Illegal operation: no pattern type: " type)]))

(define pattern-color-at _pattern-color-at)

(define pattern-transformation _pattern-transformation)

(struct _stripe _pattern () #:prefab #:type-name Stripe)

(: stripe (-> (List Color Color) Matrix Pattern))
(define (stripe colors transformation)
  (_pattern (lambda (point)
              (if (= 0 (remainder (exact-floor (tuple-x point)) 2)) (first colors) (second colors))) transformation))
