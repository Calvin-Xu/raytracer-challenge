#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "color.rkt")

(struct point-light ([position : Point] [intensity : Color]) #:prefab #:type-name PointLight)

(struct shape ([id : String] [transformation : Matrix]) #:prefab #:type-name Shape)
(struct _sphere shape () #:prefab #:type-name Sphere)

(: sphere (->* (String) (Matrix) Shape))
(define (sphere id [transformation id-mat-4])
  (_sphere id transformation))

(: set-transformation (-> (-> String Matrix Shape) Shape Matrix Shape))
(define (set-transformation constructor val trans)
  (constructor (shape-id val) trans))

(: normal-at (-> Shape Point Vector))
(define (normal-at sphere world-point)
  (let* ([trans : Matrix (shape-transformation sphere)]
         [obj-pt : Point (assert (mat-t* (inverse trans) world-point) point?)]
         [obj-norm : Vector (assert (tuple- obj-pt (pt 0. 0. 0.)) vect?)]
         [world-norm : Tuple (mat-t* (transpose (inverse trans)) obj-norm)])
    (norm (vec (tuple-x world-norm) (tuple-y world-norm) (tuple-z world-norm)))))

(: reflect (-> Vector Vector Vector))
(define (reflect in normal)
  (assert (tuple- in (tuple* normal (* 2 (dot* in normal)))) vect?))
