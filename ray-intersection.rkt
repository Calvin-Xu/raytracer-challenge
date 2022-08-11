#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "shapes.rkt")

(struct ray ([origin : Point] [direction : Vector]) #:prefab #:type-name Ray)

(: pos (-> Ray Float Point))
(define (pos ray t)
  (assert (tuple+ (ray-origin ray) (tuple* (ray-direction ray) t)) point?))

(: intersect (-> Shape Ray (Listof Intersection)))
(define (intersect shape ray)
  (let* ([center-to-ray : Vector
          (cast (tuple- (ray-origin ray) (pt 0. 0. 0.)) Vector)]
         [a : Float (dot* (ray-direction ray) (ray-direction ray))]
         [b : Float (* 2 (dot* (ray-direction ray) center-to-ray))]
         [c : Float (- (dot* center-to-ray center-to-ray) 1)]
         [discriminant : Float (- (sqr b) (* 4 a c))]
         [solution : (-> (U '+ '-) Float)
          (lambda (sign)
            (cast (/ ((if (eq? sign '-) - +) (- b) (sqrt discriminant)) (* 2 a)) Float))])
    (if (< discriminant 0)
        '()
        (list (intersection (solution '-) shape) (intersection (solution '+) shape)))))

(struct intersection ([t : Float] [obj : Shape]) #:prefab #:type-name Intersection)
