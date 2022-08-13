#lang typed/racket
(provide (except-out (all-defined-out) color-op))
(require "tuples.rkt")

(struct color ([r : Float] [g : Float] [b : Float]) #:prefab #:type-name Color)

(: color->string (->* (Color) (Exact-Nonnegative-Integer) String))
(define (color->string color [max_color_val 255])
  (: scale (-> Float Integer))
  (define (scale frac)
    (exact-round (cast (* (max 0 (min 1.0 frac)) max_color_val) Float)))
  (string-append (number->string (scale (color-r color)))
                 " "
                 (number->string (scale (color-g color)))
                 " "
                 (number->string (scale (color-b color)))
                 " "))

(define-syntax-rule (check-color= c1 c2)
  (unless (and (f= (color-r c1) (color-r c2))
               (f= (color-g c1) (color-g c2))
               (f= (color-b c1) (color-b c2)))
    (printf "Failure: colors not equal ~v, ~v\n" c1 c2)))

(: color-op (-> (-> Float Float * Float) Color Color Color))
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

(: color* (-> Color (U Color Float) Color))
(define (color* c arg)
  (color-op * c (if (color? arg) arg (color arg arg arg))))
