#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../color.rkt"
         "../canvas.rkt"
         "../matrix.rkt"
         "../transform.rkt"
         "../ray.rkt"
         "../intersect.rkt"
         "../light.rkt"
         "../shading.rkt"
         "../material.rkt"
         "../shapes.rkt"
         "../world.rkt"
         "../camera.rkt")

(define-syntax-rule (check-tuple= t1 t2)
  (check-true (and (f= (tuple-x t1) (tuple-x t2))
                   (f= (tuple-y t1) (tuple-y t2))
                   (f= (tuple-z t1) (tuple-z t2))
                   (f= (tuple-w t1) (tuple-w t2)))
              (format "Failure: tuples not equal ~a ~a" t1 t2)))

(define-syntax-rule (check-color= c1 c2)
  (check-true
   (and (f= (color-r c1) (color-r c2))
        (f= (color-g c1) (color-g c2))
        (f= (color-b c1) (color-b c2)))
   (format "Failure: colors not equal ~a ~a" c1 c2)))

(define planes-test
  (test-suite "Planes"
              (test-case "The normal of a plane is constant everywhere"
                         (define p (plane "p"))
                         (check-tuple= ((shape-normal-at p) (pt 0. 0. 0.)) (vec 0. 1. 0.))
                         (check-tuple= ((shape-normal-at p) (pt 10. 0. -10.)) (vec 0. 1. 0.))
                         (check-tuple= ((shape-normal-at p) (pt -5. 0. 150.)) (vec 0. 1. 0.)))
              (test-case "Intersect with a ray parallel to the plane"
                         (define p (plane "p"))
                         (define r (ray (pt 0. 10. 0.) (vec 0. 0. 1.)))
                         (check-equal? ((shape-intersect p) r) '()))
              (test-case "Intersect with a coplanar ray"
                         (define p (plane "p"))
                         (define r (ray (pt 0. 0. 0.) (vec 0. 0. 1.)))
                         (check-equal? ((shape-intersect p) r) '()))
              (test-case "A ray intersecting a plane from above"
                         (define p (plane "p"))
                         (define r (ray (pt 0. 1. 0.) (vec 0. -1. 0.)))
                         (check-equal? ((shape-intersect p) r) '(1.)))
              (test-case "A ray intersecting a plane from below"
                         (define p (plane "p"))
                         (define r (ray (pt 0. -1. 0.) (vec 0. 1. 0.)))
                         (check-equal? ((shape-intersect p) r) '(1.)))))

(run-tests planes-test)
