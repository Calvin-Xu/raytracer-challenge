#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../color.rkt"
         "../matrix.rkt"
         "../transform.rkt"
         "../ray.rkt"
         "../intersect.rkt"
         "../light.rkt"
         "../shading.rkt"
         "../material.rkt"
         "../shapes.rkt")

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
               (define s (sphere "s" #:transformation (translate 0. 1. 0.)))
               (define n (normal-at s (pt 0. 1.70711 -0.70711)))
               (check-tuple= n (vec 0. 0.70711 -0.70711)))
    (test-case
     "Computing the normal on a transformed sphere"
     (define s (sphere "s" #:transformation (transformation (rotate 'z (/ pi 5)) (scale 1. 0.5 1.))))
     (define n (normal-at s (pt 0. (/ (sqrt 2.) 2.) (- (/ (sqrt 2.) 2.)))))
     (check-tuple= n (vec 0. 0.97014 -0.24254))))
   (test-suite "Reflection Vectors"
               (test-case "Reflection a vector approaching at 45 deg"
                          (define v (vec 1. -1. 0.))
                          (define n (vec 0. 1. 0.))
                          (define r (reflect v n))
                          (check-tuple= r (vec 1. 1. 0.)))
               (test-case "Reflecting a vector off a slanted surface"
                          (define v (vec 0. -1. 0.))
                          (define n (vec (/ (sqrt 2.) 2.) (/ (sqrt 2.) 2.) 0.))
                          (define r (reflect v n))
                          (check-tuple= r (vec 1. 0. 0.))))
   (test-suite
    "The Phong Reflection Model"
    (test-case "A point light has a position and intensity"
               (define intensity (color 1. 1. 1.))
               (define position (pt 0. 0. 0.))
               (define light (point-light "l" position intensity))
               (check-equal? (light-position light) position)
               (check-equal? (light-intensity light) intensity))
    (test-case "The default material"
               (define m (make-material))
               (check-equal? (material-color m) (color 1. 1. 1.))
               (check-equal? (material-ambient m) 0.1)
               (check-equal? (material-diffuse m) 0.9)
               (check-equal? (material-specular m) 0.9)
               (check-equal? (material-shininess m) 200.))
    ;; (test-case "A sphere has a default material"
    ;;            (define s (sphere "s"))
    ;;            (check-equal? (shape-material s) (make-material)))
    (test-case "A sphere may be assigned a material"
               (define s (sphere "s" #:material (make-material #:ambient 1.)))
               (check-equal? (material-ambient (shape-material s)) 1.))
    (let ([m (make-material)] [pos (pt 0. 0. 0.)] [s (sphere "s")])
      (test-case "Lighting with the eye between the light and the surface"
                 (define eyev (vec 0. 0. -1.))
                 (define normalv (vec 0. 0. -1.))
                 (define light (point-light "l" (pt 0. 0. -10.) (color 1. 1. 1.)))
                 (check-color= (phong m s light pos eyev normalv) (color 1.9 1.9 1.9)))
      (test-case "Lighting with the eye between the light and the surface, eye offset 45 deg"
                 (define eyev (vec 0. (/ (sqrt 2.) 2.) (- (/ (sqrt 2.) 2.))))
                 (define normalv (vec 0. 0. -1.))
                 (define light (point-light "l" (pt 0. 0. -10.) (color 1. 1. 1.)))
                 (check-color= (phong m s light pos eyev normalv) (color 1.0 1.0 1.0)))
      (test-case "Lighting with eye opposite surface, light offset 45 deg"
                 (define eyev (vec 0. 0. -1.))
                 (define normalv (vec 0. 0. -1.))
                 (define light (point-light "l" (pt 0. 10. -10.) (color 1. 1. 1.)))
                 (check-color= (phong m s light pos eyev normalv) (color 0.7364 0.7364 0.7364)))
      (test-case "Lighting with eye in the path of the reflection vector"
                 (define eyev (vec 0. (- (/ (sqrt 2.) 2.)) (- (/ (sqrt 2.) 2.))))
                 (define normalv (vec 0. 0. -1.))
                 (define light (point-light "l" (pt 0. 10. -10.) (color 1. 1. 1.)))
                 (check-color= (phong m s light pos eyev normalv) (color 1.6364 1.6364 1.6364)))
      (test-case "Lighting with the light behind the surface"
                 (define eyev (vec 0. 0. -1.))
                 (define normalv (vec 0. 0. -1.))
                 (define light (point-light "l" (pt 0. 0. 10.) (color 1. 1. 1.)))
                 (check-color= (phong m s light pos eyev normalv) (color 0.1 0.1 0.1)))))))

(run-tests light-and-shading-test)
