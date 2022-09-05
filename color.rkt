#lang typed/racket
(provide (except-out (all-defined-out) color-op scalar-helper))
(require typed/racket/flonum)

(struct color ([r : Float] [g : Float] [b : Float]) #:prefab #:type-name Color)

(: color->string (->* (Color) (Exact-Nonnegative-Integer) String))
(define (color->string color [max-color-val 255])
  (: scale (-> Float Integer))
  (define (scale frac)
    (exact-round (fl* (flmax 0. (flmin 1.0 frac)) (exact->inexact max-color-val))))
  (string-append (number->string (scale (color-r color)))
                 " "
                 (number->string (scale (color-g color)))
                 " "
                 (number->string (scale (color-b color)))
                 " "))

(: color-op (-> (-> Float Float * Float) Color Color Color))
(define (color-op op c1 c2)
  (color (op (color-r c1) (color-r c2))
         (op (color-g c1) (color-g c2))
         (op (color-b c1) (color-b c2))))

(: color+ (-> Color Color Color))
(define (color+ c1 c2)
  (color-op + c1 c2))

(: colors+ (-> Color * Color))
(define (colors+ . colors)
  (foldl color+ (color 0. 0. 0.) colors))

(: color- (-> Color Color Color))
(define (color- c1 c2)
  (color-op - c1 c2))

(: scalar-helper (-> (U Color Float) Color))
  (define (scalar-helper arg)
    (if (color? arg) arg (color arg arg arg)))

(: color* (-> (U Color Float) (U Color Float) Color))
(define (color* arg1 arg2)
  (color-op * (scalar-helper arg1) (scalar-helper arg2)))

(: color/ (-> Color (U Color Float) Color))
(define (color/ arg1 arg2)
  (color-op / (scalar-helper arg1) (scalar-helper arg2)))

(define black (color 0. 0. 0.))

(define white (color 1. 1. 1.))
