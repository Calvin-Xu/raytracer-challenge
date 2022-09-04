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

(define dummy (sphere "s"))

(: test-pattern (->* () (Matrix) Pattern))
(define (test-pattern [transformation id-mat-4])
  (_pattern (lambda (point) (color (tuple-x point) (tuple-y point) (tuple-z point)))
            transformation
            (inverse transformation)))

(define patterns-test
  (test-suite
   "Patterns"
   (test-suite "Making a Striped Pattern"
               (test-case "A stripe pattern is constant in y"
                          (define p
                            (pattern 'stripe
                              (list white black)))
                          (check-equal? (pattern-at p dummy (pt 0. 0. 0.)) white)
                          (check-equal? (pattern-at p dummy (pt 0. 1. 0.)) white)
                          (check-equal? (pattern-at p dummy (pt 0. 2. 0.)) white))
               (test-case "A stripe pattern is constant in z"
                          (define p
                            (pattern 'stripe
                              (list white black)))
                          (check-equal? (pattern-at p dummy (pt 0. 0. 0.)) white)
                          (check-equal? (pattern-at p dummy (pt 0. 0. 1.)) white)
                          (check-equal? (pattern-at p dummy (pt 0. 0. 2.)) white))
               (test-case "A stripe pattern alternates in x"
                          (define p
                            (pattern 'stripe
                              (list white black)))
                          (check-equal? (pattern-at p dummy (pt 0. 0. 0.)) white)
                          (check-equal? (pattern-at p dummy (pt 0.9 0. 0.)) white)
                          (check-equal? (pattern-at p dummy (pt 1. 0. 0.)) black)
                          (check-equal? (pattern-at p dummy (pt -0.1 0. 0.)) black)
                          (check-equal? (pattern-at p dummy (pt -1. 0. 0.)) black)
                          (check-equal? (pattern-at p dummy (pt -1.1 0. 0.)) white))
               (test-case "Lighting with a pattern applied"
                          (define m
                            (make-material #:ambient 1.
                                           #:diffuse 0.
                                           #:specular 0.
                                           #:pattern (pattern 'stripe
                                                       (list white black))))
                          (define eyev (vec 0. 0. -1.))
                          (define normalv (vec 0. 0. -1.))
                          (define light (point-light "l" (pt 0. 0. -10.) white))
                          (check-color= (phong m dummy light (pt 0.9 0. 0.) eyev normalv #f) white)
                          (check-color= (phong m dummy light (pt 1.1 0. 0.) eyev normalv #f) black)))
   (test-suite "Transforming Patterns"
               (test-case "Stripes with an object transformation"
                          (define s (sphere "s" #:transformation (scale 2. 2. 2.)))
                          (define p
                            (pattern 'stripe
                              (list white black)))
                          (check-color= (pattern-at p s (pt 1.5 0. 0.)) white))
               (test-case "Stripes with a pattern transformation"
                          (define s (sphere "s"))
                          (define p
                            (pattern 'stripe
                              (list white black)
                              #:transformation (scale 2. 2. 2.)))
                          (check-color= (pattern-at p s (pt 1.5 0. 0.)) white))
               (test-case "Stripes with both an object and a pattern transformation"
                          (define s (sphere "s" #:transformation (scale 2. 2. 2.)))
                          (define p
                            (pattern 'stripe
                              (list white black)
                              #:transformation (translate 0.5 0. 0.)))
                          (check-color= (pattern-at p s (pt 2.5 0. 0.)) white)))
   (test-suite "Generalizing Patterns"
               (test-case "A pattern with an object transformation"
                          (define s (sphere "s" #:transformation (scale 2. 2. 2.)))
                          (define t (test-pattern))
                          (check-color= (pattern-at t s (pt 2. 3. 4.)) (color 1. 1.5 2.)))
               (test-case "A pattern with a pattern transformation"
                          (define s (sphere "s"))
                          (define t (test-pattern (scale 2. 2. 2.)))
                          (check-color= (pattern-at t s (pt 2. 3. 4.)) (color 1. 1.5 2.)))
               (test-case "A pattern with both an object and a pattern transformation"
                          (define s (sphere "s" #:transformation (scale 2. 2. 2.)))
                          (define t (test-pattern (translate 0.5 1. 1.5)))
                          (check-color= (pattern-at t s (pt 2.5 3. 3.5)) (color 0.75 0.5 0.25))))
   (test-suite "Making a Gradient Pattern"
               (test-case "A gradient linearly interpolates between colors"
                          (define p
                            (pattern 'gradient
                              (list white black)))
                          (check-color= (pattern-at p dummy (pt 0. 0. 0.)) white)
                          (check-color= (pattern-at p dummy (pt 0.25 0. 0.)) (color 0.75 0.75 0.75))
                          (check-color= (pattern-at p dummy (pt 0.5 0. 0.)) (color 0.5 0.5 0.5))
                          (check-color= (pattern-at p dummy (pt 0.75 0. 0.)) (color 0.25 0.25 0.25))))
   (test-suite "Making a Ring Pattern"
               (test-case "A ring should extend in both x and z"
                          (define p
                            (pattern 'ring
                              (list white black)))
                          (check-color= (pattern-at p dummy (pt 0. 0. 0.)) white)
                          (check-color= (pattern-at p dummy (pt 1. 0. 0.)) black)
                          (check-color= (pattern-at p dummy (pt 0. 0. 1.)) black)
                          (check-color= (pattern-at p dummy (pt 0.708 0. 0.708)) black)))
   (test-suite "Making a 3D Checker Pattern"
               (let ([p (pattern 'checker
                          (list white black))])
                 (test-case "Checkers should repeat in x"
                            (check-color= (pattern-at p dummy (pt 0. 0. 0.)) white)
                            (check-color= (pattern-at p dummy (pt 0.99 0. 0.)) white)
                            (check-color= (pattern-at p dummy (pt 1.01 0. 0.)) black))
                 (test-case "Checkers should repeat in y"
                            (check-color= (pattern-at p dummy (pt 0. 0. 0.)) white)
                            (check-color= (pattern-at p dummy (pt 0. 0.99 0.)) white)
                            (check-color= (pattern-at p dummy (pt 0. 1.01 0.)) black))
                 (test-case "Checkers should repeat in z"
                            (check-color= (pattern-at p dummy (pt 0. 0. 0.)) white)
                            (check-color= (pattern-at p dummy (pt 0. 0. 0.99)) white)
                            (check-color= (pattern-at p dummy (pt 0. 0. 1.01)) black))))))

(run-tests patterns-test)
