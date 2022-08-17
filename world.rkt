#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "matrix.rkt")
(require "transform.rkt")
(require "ray.rkt")
(require "material.rkt")
(require "shapes.rkt")
(require "lighting.rkt")

(struct world
        ([objects : (Immutable-HashTable String Shape)] [lights : (Immutable-HashTable String Light)])
  #:prefab
  #:type-name World)

(: add-object (-> World Shape * World))
(define (add-object w . objects)
  (for/fold ([objs : (Immutable-HashTable String Shape) (world-objects w)]
             #:result (world objs (world-lights w)))
            ([obj (in-list objects)])
    (hash-set (world-objects w) (shape-id obj) obj)))

(: add-light (-> World Light * World))
(define (add-light w . lights)
  (for/fold ([luces : (Immutable-HashTable String Light) (world-lights w)]
             #:result (world (world-objects w) luces))
            ([lux (in-list lights)])
    (hash-set (world-lights w) (light-id lux) lux)))

(define make-world (world (make-immutable-hash) (make-immutable-hash)))

(define default-world
  (let* ([w1 make-world]
         [w2 (add-light w1 (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))]
         [w3 (add-object w2
                         (sphere "outer concentric sphere"
                                 #:material (make-material #:color (color 0.8 1.0 0.6)
                                                              #:diffuse 0.7
                                                              #:specular 0.2)))]
         [w4 (add-object w3 (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5)))])
    w4))
