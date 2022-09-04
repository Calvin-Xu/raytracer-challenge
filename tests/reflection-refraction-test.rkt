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
         "../camera.rkt"
         "../patterns.rkt")

(define-syntax-rule (check-f= x y)
  (check-= x y EPSILON))

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

(: test-pattern (->* () (Matrix) Pattern))
(define (test-pattern [transformation id-mat-4])
  (_pattern (lambda (point) (color (tuple-x point) (tuple-y point) (tuple-z point)))
            transformation
            (inverse transformation)))

(define reflection-test
  (test-suite
   "Reflection"
   (test-case "Precomputing the reflection vector"
              (define s (plane "p"))
              (define r (ray (pt 0. 1. -1.) (vec 0. (- (/ (sqrt 2.) 2.)) (/ (sqrt 2.) 2.))))
              (define i (intersection (sqrt 2.) s))
              (define comps (precomp i r))
              (check-tuple= (intersection-data-reflectv comps)
                            (vec 0. (/ (sqrt 2.) 2.) (/ (sqrt 2.) 2.))))
   (test-case
    "The reflected color for a nonreflective material"
    (define w
      (make-world (list (sphere "outer concentric sphere"
                                #:material (make-material #:color (color 0.8 1.0 0.6)
                                                          #:ambient 1.
                                                          #:diffuse 0.7
                                                          #:specular 0.2))
                        (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5)))
                  (list (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))))
    (define r (ray (pt 0. 0. 0.) (vec 0. 0. 1.)))
    (define i (intersection 1. (hash-ref (world-objects w) "inner concentric sphere")))
    (define comps (precomp i r))
    (check-color= (shade-reflection w comps 1) black))
   (test-case
    "The reflected color for a reflective material"
    (define s
      (plane "p" #:material (make-material #:reflective 0.5) #:transformation (translate 0. -1. 0.)))
    (define w (add-objects default-world s))
    (define r (ray (pt 0. 0. -3.) (vec 0. (- (/ (sqrt 2.) 2.)) (/ (sqrt 2.) 2.))))
    (define i (intersection (sqrt 2.) s))
    (define comps (precomp i r))
    (check-color= (shade-reflection w comps 1) (color 0.19033 0.23792 0.14275)))
   (test-case
    "shade-intersection with a reflective material"
    (define s
      (plane "p" #:material (make-material #:reflective 0.5) #:transformation (translate 0. -1. 0.)))
    (define w (add-objects default-world s))
    (define r (ray (pt 0. 0. -3.) (vec 0. (- (/ (sqrt 2.) 2.)) (/ (sqrt 2.) 2.))))
    (define i (intersection (sqrt 2.) s))
    (define comps (precomp i r))
    (check-color= (shade-intersection w comps) (color 0.87676 0.92434 0.82917)))
   (test-case "shade-ray with mutually reflective surfaces"
              (define w
                (make-world (list (plane "lower"
                                         #:material (make-material #:reflective 1.)
                                         #:transformation (translate 0. -1. 0.))
                                  (plane "upper"
                                         #:material (make-material #:reflective 1.)
                                         #:transformation (translate 0. 1. 0.)))
                            (list (point-light "l" (pt 0. 0. 0.) white))))
              (define r (ray (pt 0. 0. 0.) (vec 0. 1. 0.)))
              (check-color= (shade-ray w r) (color 11.4 11.4 11.4)))
   (test-case
    "The reflected color at the maximum recursive depth"
    (define s
      (plane "p" #:material (make-material #:reflective 0.5) #:transformation (translate 0. -1. 0.)))
    (define w (add-objects default-world s))
    (define r (ray (pt 0. 0. -3.) (vec 0. (- (/ (sqrt 2.) 2.)) (/ (sqrt 2.) 2.))))
    (define i (intersection (sqrt 2.) s))
    (define comps (precomp i r))
    (check-color= (shade-reflection w comps 0) black))))

(define refraction-test
  (test-suite
   "Refraction"
   (test-case "Finding n1 and n2 at various intersections"
              (define a
                (sphere "a"
                        #:transformation (scale 2. 2. 2.)
                        #:material (make-material #:transparency 1.0 #:refractive 1.5)))
              (define b
                (sphere "b"
                        #:transformation (translate 0. 0. -0.25)
                        #:material (make-material #:transparency 1.0 #:refractive 2.0)))
              (define c
                (sphere "c"
                        #:transformation (translate 0. 0. 0.25)
                        #:material (make-material #:transparency 1.0 #:refractive 2.5)))
              (define r (ray (pt 0. 0. -4.) (vec 0. 0. 1.)))
              (define xs
                (list (intersection 2. a)
                      (intersection 2.75 b)
                      (intersection 3.25 c)
                      (intersection 4.75 b)
                      (intersection 5.25 c)
                      (intersection 6. a)))
              (define n1s (list 1.0 1.5 2.0 2.5 2.5 1.5))
              (define n2s (list 1.5 2.0 2.5 2.5 1.5 1.0))
              (for ([i (in-range 0 6)])
                (let ([comps (precomp (list-ref xs i) r xs)])
                  (check-f= (intersection-data-n1 comps) (list-ref n1s i))
                  (check-f= (intersection-data-n2 comps) (list-ref n2s i)))))
   (test-case "The under point is offset below the surface"
              (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
              (define s
                (sphere "s"
                        #:transformation (translate 0. 0. 1.)
                        #:material (make-material #:transparency 1.0 #:refractive 1.5)))
              (define i (intersection 5. s))
              (define comps (precomp i r (list i)))
              (check-true (> (tuple-z (intersection-data-point- comps)) (/ EPSILON 2)))
              (check-true (< (tuple-z (intersection-data-point comps))
                             (tuple-z (intersection-data-point- comps)))))
   (test-case "The refracted color with an opaque surface"
              (define w default-world)
              (define s (hash-ref (world-objects w) "outer concentric sphere"))
              (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
              (define xs (list (intersection 4. s) (intersection 6. s)))
              (define comps (precomp (car xs) r xs))
              (check-color= (shade-refraction w comps 5) black))
   (test-case "The refracted color at the maximum recursive depth"
              (define w
                (make-world
                 (list (sphere "outer concentric sphere"
                               #:material (make-material #:color (color 0.8 1.0 0.6)
                                                         #:diffuse 0.7
                                                         #:specular 0.2
                                                         #:transparency 1.
                                                         #:refractive 1.5))
                       (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5)))
                 (list (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))))
              (define s (hash-ref (world-objects w) "outer concentric sphere"))
              (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
              (define xs (list (intersection 4. s) (intersection 6. s)))
              (define comps (precomp (car xs) r xs))
              (check-color= (shade-refraction w comps 0) black))
   (test-case
    "The refracted color under total internal reflection"
    (define w
      (make-world (list (sphere "outer concentric sphere"
                                #:material (make-material #:color (color 0.8 1.0 0.6)
                                                          #:diffuse 0.7
                                                          #:specular 0.2
                                                          #:transparency 1.
                                                          #:refractive 1.5))
                        (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5)))
                  (list (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))))
    (define s (hash-ref (world-objects w) "outer concentric sphere"))
    (define r (ray (pt 0. 0. (/ (sqrt 2.) 2.)) (vec 0. 1. 0.)))
    (define xs (list (intersection (- (/ (sqrt 2.) 2.)) s) (intersection (/ (sqrt 2.) 2.) s)))
    (define comps (precomp (cadr xs) r xs))
    (check-color= (shade-refraction w comps 5) black))
   (test-case "The refracted color with a refracted ray"
              (define w
                (make-world
                 (list (sphere "outer concentric sphere"
                               #:material (make-material #:color (color 0.8 1.0 0.6)
                                                         #:diffuse 0.7
                                                         #:specular 0.2
                                                         #:ambient 1.
                                                         #:pattern (test-pattern)))
                       (sphere "inner concentric sphere"
                               #:transformation (scale 0.5 0.5 0.5)
                               #:material (make-material #:transparency 1.0 #:refractive 1.5)))
                 (list (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))))
              (define a (hash-ref (world-objects w) "outer concentric sphere"))
              (define b (hash-ref (world-objects w) "inner concentric sphere"))
              (define r (ray (pt 0. 0. 0.1) (vec 0. 1. 0.)))
              (define xs
                (list (intersection -0.9899 a)
                      (intersection -0.4899 b)
                      (intersection 0.4899 b)
                      (intersection 0.9899 a)))
              (define comps (precomp (third xs) r xs))
              (check-color= (shade-refraction w comps 5) (color 0. 0.99889 0.04722)))
   (test-case
    "shade-intersection with a transparent material"
    (define w
      (make-world
       (list (sphere "outer concentric sphere"
                     #:material
                     (make-material #:color (color 0.8 1.0 0.6) #:diffuse 0.7 #:specular 0.2))
             (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5))
             (plane "floor"
                    #:transformation (translate 0. -1. 0.)
                    #:material (make-material #:transparency 0.5 #:refractive 1.5))
             (sphere "ball"
                     #:transformation (translate 0. -3.5 -0.5)
                     #:material (make-material #:color (color 1. 0. 0.) #:ambient 0.5)))
       (list (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))))
    (define r (ray (pt 0. 0. -3.) (vec 0. (- (/ (sqrt 2.) 2.)) (/ (sqrt 2.) 2.))))
    (define xs (list (intersection (sqrt 2.) (hash-ref (world-objects w) "floor"))))
    (define comps (precomp (car xs) r xs))
    (check-color= (shade-intersection w comps 5) (color 0.93642 0.68642 0.68642)))))

(define schlick-test
  (test-suite
   "Schlick's approximation"
   (test-case
    "The Schlick approximation under total internal reflection"
    (define s (sphere "s" #:material (make-material #:transparency 1. #:refractive 1.5)))
    (define r (ray (pt 0. 0. (/ (sqrt 2.) 2.)) (vec 0. 1. 0.)))
    (define xs (list (intersection (- (/ (sqrt 2.) 2.)) s) (intersection (/ (sqrt 2.) 2.) s)))
    (define comps (precomp (cadr xs) r xs))
    (check-f= (reflectance comps) 1.0))
   (test-case "The Schlick approximation with a perpendicular viewing angle"
              (define s (sphere "s" #:material (make-material #:transparency 1. #:refractive 1.5)))
              (define r (ray (pt 0. 0. 0.) (vec 0. 1. 0.)))
              (define xs (list (intersection -1. s) (intersection 1. s)))
              (define comps (precomp (cadr xs) r xs))
              (check-f= (reflectance comps) 0.04))
   (test-case "The Schlick approximation with small angle and n2 > n1"
              (define s (sphere "s" #:material (make-material #:transparency 1. #:refractive 1.5)))
              (define r (ray (pt 0. 0.99 -2.) (vec 0. 0. 1.)))
              (define xs (list (intersection 1.8589 s)))
              (define comps (precomp (car xs) r xs))
              (check-f= (reflectance comps) 0.48873))
   (test-case
    "shade-intersection with a reflective, transparent material"
    (define w
      (make-world
       (list
        (sphere "outer concentric sphere"
                #:material (make-material #:color (color 0.8 1.0 0.6) #:diffuse 0.7 #:specular 0.2))
        (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5))
        (plane "floor"
               #:transformation (translate 0. -1. 0.)
               #:material (make-material #:reflective 0.5 #:transparency 0.5 #:refractive 1.5))
        (sphere "ball"
                #:transformation (translate 0. -3.5 -0.5)
                #:material (make-material #:color (color 1. 0. 0.) #:ambient 0.5)))
       (list (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))))
    (define r (ray (pt 0. 0. -3.) (vec 0. (- (/ (sqrt 2.) 2.)) (/ (sqrt 2.) 2.))))
    (define xs (list (intersection (sqrt 2.) (hash-ref (world-objects w) "floor"))))
    (define comps (precomp (car xs) r xs))
    (check-color= (shade-intersection w comps 5) (color 0.93391 0.69643 0.69243)))))

(run-tests reflection-test)
(run-tests refraction-test)
(run-tests schlick-test)
