#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "color.rkt")
(require "material.rkt")

(struct shape ([id : String] [transformation : Matrix] [material : Material]) #:prefab #:type-name Shape)
(struct _sphere shape () #:prefab #:type-name Sphere)

(: sphere (->* (String) (#:transformation Matrix #:material Material) Shape))
(define (sphere id #:transformation [transformation id-mat-4] #:material [material (make-material)])
  (_sphere id transformation material))

(: set-transformation
   (-> (->* (String) (#:transformation Matrix #:material Material) Shape) Shape Matrix Shape))
(define (set-transformation constructor val trans)
  (constructor (shape-id val) #:transformation trans #:material (shape-material val)))
