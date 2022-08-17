#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../color.rkt"
         "../matrix.rkt"
         "../transform.rkt"
         "../ray.rkt"
         "../intersect.rkt"
         "../lighting.rkt"
         "../material.rkt"
         "../shapes.rkt"
         "../world.rkt")

(define-syntax-rule (check-tuple= t1 t2)
    (unless (and (f= (tuple-x t1) (tuple-x t2))
                 (f= (tuple-y t1) (tuple-y t2))
                 (f= (tuple-z t1) (tuple-z t2))
                 (f= (tuple-w t1) (tuple-w t2)))
      (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define scene-test
  (test-suite
   "Making a Scene"
   (test-suite
    "Building a World"
    (test-case "Creating a world"
               (define w make-world)
               (check-equal? (hash-values (world-objects w)) '())
               (check-equal? (hash-values (world-lights w)) '()))
    (test-case
     "The default world"
     (define w default-world)
     (check-equal? (hash-ref (world-lights w) "default light")
                   (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))
     (check-equal?
      (hash-ref (world-objects w) "outer concentric sphere")
      (sphere "outer concentric sphere"
              #:material (make-material #:color (color 0.8 1.0 0.6) #:diffuse 0.7 #:specular 0.2)))
     (check-equal? (hash-ref (world-objects w) "inner concentric sphere")
                   (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5))))
    (test-case "Intersect a world with a ray"
               (define w default-world)
               (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
               (define xs (intersect-world w r))
               (check-equal? (length xs) 4)
               (check-equal? (intersection-t (first xs)) 4.)
               (check-equal? (intersection-t (second xs)) 4.5)
               (check-equal? (intersection-t (third xs)) 5.5)
               (check-equal? (intersection-t (fourth xs)) 6.))
    (test-case "Precomputing the state of an intersection")
    (test-case "The hit, when an intersection occurs on the outside")
    (test-case "The hit, when an intersection occurs on the inside")
    (test-case "Shading an intersection")
    (test-case "Shading an intersection from the inside")
    (test-case "The color when a ray misses")
    (test-case "The color when a ray hits")
    (test-case "The color with an intersection behind the ray"))
   (test-suite "Defining a View Transformation")
   (test-suite "Implementing a Camera")
   (test-suite "Putting it Together")))

(run-tests scene-test)
