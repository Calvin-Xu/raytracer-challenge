#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "transform.rkt")
(require "material.rkt")
(require "shapes.rkt")
(require "light.rkt")

(struct world
        ([objects : (Immutable-HashTable String Shape)] [lights : (Immutable-HashTable String Light)])
  #:prefab
  #:type-name World)

(: add-objects (-> World Shape * World))
(define (add-objects w . objects)
  (for/fold ([objs : (Immutable-HashTable String Shape) (world-objects w)]
             #:result (world objs (world-lights w)))
            ([obj (in-list objects)])
    (hash-set objs (shape-id obj) obj)))

(: add-lights (-> World Light * World))
(define (add-lights w . lights)
  (for/fold ([luces : (Immutable-HashTable String Light) (world-lights w)]
             #:result (world (world-objects w) luces))
            ([lux (in-list lights)])
    (hash-set luces (light-id lux) lux)))

(: make-world (->* () ((Listof Shape) (Listof Light)) World))
(define (make-world [objects '()] [lights '()])
  (let ([empty : World (world (make-immutable-hash) (make-immutable-hash))])
    (apply add-objects (apply add-lights empty lights) objects)))

(define default-world
  (let* ([w1 (make-world)]
         [w2 (add-lights w1 (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))]
         [w3 (add-objects w2
                         (sphere "outer concentric sphere"
                                 #:material (make-material #:color (color 0.8 1.0 0.6)
                                                              #:diffuse 0.7
                                                              #:specular 0.2)))]
         [w4 (add-objects w3 (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5)))])
    w4))
