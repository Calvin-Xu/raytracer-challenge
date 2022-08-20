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

(define shadow-test
  (test-suite
   "Shadows"
   (test-suite "Lighting in Shadows"
               (test-case "Lighting with the surface in shadow"
                          (define eyev (vec 0. 0. -1.))
                          (define normalv (vec 0. 0. -1.))
                          (define light (point-light "l" (pt 0. 0. -10.) (color 1. 1. 1.)))
                          (define in-shadow #t)
                          (check-color=
                           (phong (make-material) light (pt 0. 0. 0.) eyev normalv in-shadow)
                           (color 0.1 0.1 0.1))))
   (test-suite "Testing for Shadows"
               (test-case "There is no shadow when nothing is collinear with point and light"
                          (define w default-world)
                          (define p (pt 0. 10. 0.))
                          (check-false (is-shadowed w (car (hash-values (world-lights w))) p)))
               (test-case "The shadow when an object is between the point and the light"
                          (define w default-world)
                          (define p (pt 10. -10. 10.))
                          (check-true (is-shadowed w (car (hash-values (world-lights w))) p)))
               (test-case "There is no shadow when an object is behind the light"
                          (define w default-world)
                          (define p (pt -20. 20. -20.))
                          (check-false (is-shadowed w (car (hash-values (world-lights w))) p)))
               (test-case "There is no shadow when an object is behind the point"
                          (define w default-world)
                          (define p (pt -2. 2. -2.))
                          (check-false (is-shadowed w (car (hash-values (world-lights w))) p))))
   (test-suite
    "Rendering Shadows"
    (test-case "shade-hit is given an intersection in shadow"
               (define s2 (sphere "s2" #:transformation (translate 0. 0. 0.)))
               (define w
                 (make-world (list (sphere "s1") s2)
                             (list (point-light "l1" (pt 0. 0. -10.) (color 1. 1. 1.)))))
               (define r (ray (pt 0. 0. 5.) (vec 0. 0. 1.)))
               (define i (intersection 4. s2))
               (define c (shade-intersection w (precomp i r)))
               (check-color= c (color 0.1 0.1 0.1)))
    (test-case "The hit should offset the point"
               (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
               (define s (sphere "s" #:transformation (translate 0. 0. 1.)))
               (define i (intersection 5. s))
               (define comps (precomp i r))
               (check-true (< (tuple-z (intersection-data-over-pt comps)) (- (/ 0.00001 2))))
               (check-true (> (tuple-z (intersection-data-point comps))
                              (tuple-z (intersection-data-over-pt comps))))))))

(run-tests shadow-test)
