#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../matrix.rkt"
         "../transform.rkt"
         "../ray-intersection.rkt"
         "../shapes.rkt")

(define-syntax-rule (check-tuple= t1 t2)
  (unless (and (f= (tuple-x t1) (tuple-x t2))
               (f= (tuple-y t1) (tuple-y t2))
               (f= (tuple-z t1) (tuple-z t2))
               (f= (tuple-w t1) (tuple-w t2)))
    (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define ray-intersection-test
  (test-suite
   "Ray-Sphere Intersections"
   (test-suite "Creating Rays"
               (test-case "Creating and querying a ray"
                          (define origin (pt 1. 2. 3.))
                          (define direction (vec 4. 5. 6.))
                          (define r (ray origin direction))
                          (check-tuple= (ray-origin r) origin)
                          (check-tuple= (ray-direction r) direction))
               (test-case "Computing a point from a distance"
                          (define r (ray (pt 2. 3. 4.) (vec 1. 0. 0.)))
                          (check-tuple= (pos r 0.) (pt 2. 3. 4.))
                          (check-tuple= (pos r 1.) (pt 3. 3. 4.))
                          (check-tuple= (pos r -1.) (pt 1. 3. 4.))
                          (check-tuple= (pos r 2.5) (pt 4.5 3. 4.))))
   (test-suite "Intersecting Rays with Spheres"
               (test-case "A ray intersects a sphere at two points"
                          (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
                          (define s (sphere "s"))
                          (define xs (intersect s r))
                          (check-equal? xs (list (intersection 4. s) (intersection 6. s))))
               (test-case "A ray intersects a sphere at a tangent"
                          (define r (ray (pt 0. 1. -5.) (vec 0. 0. 1.)))
                          (define s (sphere "s"))
                          (define xs (intersect s r))
                          (check-equal? xs (list (intersection 5. s) (intersection 5. s))))
               (test-case "A ray misses a sphere"
                          (define r (ray (pt 0. 2. -5.) (vec 0. 0. 1.)))
                          (define s (sphere "s"))
                          (define xs (intersect s r))
                          (check-equal? xs '()))
               (test-case "A ray originates inside a sphere"
                          (define r (ray (pt 0. 0. 0.) (vec 0. 0. 1.)))
                          (define s (sphere "s"))
                          (define xs (intersect s r))
                          (check-equal? xs (list (intersection -1. s) (intersection 1. s))))
               (test-case "A sphere is behind a ray"
                          (define r (ray (pt 0. 0. 5.) (vec 0. 0. 1.)))
                          (define s (sphere "s"))
                          (define xs (intersect s r))
                          (check-equal? xs (list (intersection -6. s) (intersection -4. s)))))
   (test-suite "Tracking Intersections"
               (test-case "An intersection encapsulates t and object"
                          (define s (sphere "s"))
                          (define i (intersection 3.5 s))
                          (check-equal? (intersection-t i) 3.5)
                          (check-equal? (intersection-obj i) s))
               (test-case "Intersect sets the object on the intersection"
                          (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
                          (define s (sphere "s"))
                          (define xs (intersect s r))
                          (check-equal? (length xs) 2)
                          (check-equal? (intersection-obj (list-ref xs 0)) s)
                          (check-equal? (intersection-obj (list-ref xs 1)) s)))))

(run-tests ray-intersection-test)
