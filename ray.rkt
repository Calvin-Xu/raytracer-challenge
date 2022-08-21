#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "transform.rkt")

(struct ray ([origin : Point] [direction : Vector]) #:prefab #:type-name Ray)

(: pos (-> Ray Float Point))
(define (pos ray t)
  (assert (tuple+ (ray-origin ray) (tuple* (ray-direction ray) t)) point?))

(: transform-ray (-> Ray Matrix * Ray))
(define (transform-ray r . transformations)
  (ray (assert (mat-t* (apply transformation transformations) (ray-origin r)) point?)
       (assert (mat-t* (apply transformation transformations) (ray-direction r)) vect?)))
