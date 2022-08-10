#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../matrix.rkt"
         "../transform.rkt"
         "../ray-intersection.rkt")

(define-syntax-rule (check-tuple= t1 t2)
  (unless (and (f= (tuple-x t1) (tuple-x t2))
               (f= (tuple-y t1) (tuple-y t2))
               (f= (tuple-z t1) (tuple-z t2))
               (f= (tuple-w t1) (tuple-w t2)))
    (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define ray-intersection-test
  (test-suite "Ray-Sphere Intersections"
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
                                     (check-tuple= (pos r 2.5) (pt 4.5 3. 4.))))))

(run-tests ray-intersection-test)
