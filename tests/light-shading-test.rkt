#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../matrix.rkt"
         "../transform.rkt"
         "../ray.rkt"
         "../shapes.rkt")

(define-syntax-rule (check-tuple= t1 t2)
    (unless (and (f= (tuple-x t1) (tuple-x t2))
                 (f= (tuple-y t1) (tuple-y t2))
                 (f= (tuple-z t1) (tuple-z t2))
                 (f= (tuple-w t1) (tuple-w t2)))
      (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define light-and-shading-test
  (test-suite
   "Light and Shading"
   (test-suite
    "Surface Normals"
    (test-case "The normal on a sphere at a point on the x axis"
               (define s (sphere "s"))
               (define n (normal-at s (pt 1. 0. 0.)))
               (check-tuple= n (vec 1. 0. 0.)))
    (test-case "The normal on a sphere at a point on the y axis"
               (define s (sphere "s"))
               (define n (normal-at s (pt 0. 1. 0.)))
               (check-tuple= n (vec 0. 1. 0.)))
    (test-case "The normal on a sphere at a point on the z axis"
               (define s (sphere "s"))
               (define n (normal-at s (pt 0. 0. 1.)))
               (check-tuple= n (vec 0. 0. 1.)))
    (test-case "The normal on a sphere at a nonaxial point"
               (define s (sphere "s"))
               (define n (normal-at s (pt (/ (sqrt 3) 3.) (/ (sqrt 3) 3.) (/ (sqrt 3) 3.))))
               (check-tuple= n (vec (/ (sqrt 3) 3.) (/ (sqrt 3) 3.) (/ (sqrt 3) 3.))))
    (test-case "The normal is a normalized vector"
               (define s (sphere "s"))
               (define n (normal-at s (pt 1. 0. 0.)))
               (check-tuple= n (norm n)))
    (test-case "Computing the normal on a translated sphere"
               (define s (sphere "s" (translate 0. 1. 0.)))
               (define n (normal-at s (pt 0. 1.70711 -0.70711)))
               (check-tuple= n (vec 0. 0.70711 -0.70711)))
    (test-case "Computing the normal on a transformed sphere"
               (define s (sphere "s" (transformation (rotate 'z (/ pi 5)) (scale 1. 0.5 1.))))
               (define n (normal-at s (pt 0. (/ (sqrt 2.) 2.) (- (/ (sqrt 2.) 2.)))))
               (check-tuple= n (vec 0. 0.97014 -0.24254))))))

(run-tests light-and-shading-test)
