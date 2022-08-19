#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "matrix.rkt")
(require "shapes.rkt")

(struct light ([id : String] [position : Point] [intensity : Color]) #:prefab #:type-name Light)
(struct point-light light () #:prefab #:type-name PointLight)

(: reflect (-> Vector Vector Vector))
(define (reflect in normal)
  (assert (tuple- in (tuple* normal (* 2 (dot* in normal)))) vect?))

(: normal-at (-> Shape Point Vector))
(define (normal-at sphere world-point)
  (let* ([trans : Matrix (shape-transformation sphere)]
         [obj-pt : Point (assert (mat-t* (inverse trans) world-point) point?)]
         [obj-norm : Vector (assert (tuple- obj-pt (pt 0. 0. 0.)) vect?)]
         [world-norm : Tuple (mat-t* (transpose (inverse trans)) obj-norm)])
    (norm (vec (tuple-x world-norm) (tuple-y world-norm) (tuple-z world-norm)))))
