#lang typed/racket
(require "tuples.rkt")

(struct _color tuple () #:prefab #:type-name Color)

(: color (->* (Real Real Real) (Real) Color))
(define (color red green blue [alpha 1])
  (_color red green blue alpha))

(define color? _color?)

(: color-r (-> Color Real))
(define (color-r color)
  (tuple-x color))

(: color-g (-> Color Real))
(define (color-g color)
  (tuple-y color))

(: color-b (-> Color Real))
(define (color-b color)
  (tuple-z color))

(: color-a (-> Color Real))
(define (color-a color)
  (tuple-w color))

(: color-op (-> (-> Real Real * Real) Color Color Color))
(define (color-op op c1 c2)
  (color (op (color-r c1) (color-r c2))
         (op (color-g c1) (color-g c2))
         (op (color-b c1) (color-b c2))))

(: color+ (-> Color Color Color))
(define (color+ c1 c2)
  (color-op + c1 c2))

(: color- (-> Color Color Color))
(define (color- c1 c2)
  (color-op - c1 c2))

(: color* (-> Color (U Color Real) Color))
(define (color* c arg)
  (color-op * c (if (color? arg) arg (color arg arg arg))))

(provide (except-out (all-defined-out) color-op))
