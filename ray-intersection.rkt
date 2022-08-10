#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")

(struct ray ([origin : Point] [direction : Vector]) #:prefab #:type-name Ray)

(: pos (-> Ray Float Point))
(define (pos ray t)
  (assert (tuple+ (ray-origin ray) (tuple* (ray-direction ray) t)) point?))
