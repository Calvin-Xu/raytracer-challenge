#lang typed/racket
(provide (all-defined-out))
(require "matrix.rkt")

(struct shape ([id : String] [transformation : Matrix]) #:prefab #:type-name Shape)
(struct _sphere shape () #:prefab #:type-name Sphere)

(: sphere (->* (String) (Matrix) Shape))
(define (sphere id [transformation id-mat-4])
  (_sphere id transformation))

(: set-transformation (-> (-> String Matrix Shape) Shape Matrix Shape))
(define (set-transformation constructor val trans)
  (constructor (shape-id val) trans))