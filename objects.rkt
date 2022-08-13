#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "color.rkt")

(struct point-light ([position : Point] [intensity : Color]) #:prefab #:type-name PointLight)

(struct material
        ([color : Color] [ambient : Float] [diffuse : Float] [specular : Float] [shininess : Float])
  #:prefab
  #:type-name Material)

(:
 default-material
 (->* () (#:color Color #:ambient Float #:diffuse Float #:specular Float #:shininess Float) Material))
(define (default-material #:color [color (color 1. 1. 1.)]
                          #:ambient [ambient 0.1]
                          #:diffuse [diffuse 0.9]
                          #:specular [specular 0.9]
                          #:shininess [shininess 200.])
  (material color ambient diffuse specular shininess))

(struct shape ([id : String] [transformation : Matrix] [material : Material]) #:prefab #:type-name Shape)
(struct _sphere shape () #:prefab #:type-name Sphere)

(: sphere (->* (String) (#:transformation Matrix #:material Material) Shape))
(define (sphere id #:transformation [transformation id-mat-4] #:material [material (default-material)])
  (_sphere id transformation material))

(: set-transformation
   (-> (->* (String) (#:transformation Matrix #:material Material) Shape) Shape Matrix Shape))
(define (set-transformation constructor val trans)
  (constructor (shape-id val) #:transformation trans #:material (shape-material val)))

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
