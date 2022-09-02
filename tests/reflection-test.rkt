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
    (define i (intersection 1. (hash-ref (world-objects w) "outer concentric sphere")))
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

(run-tests reflection-test)
