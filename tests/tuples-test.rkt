#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt")

(define-syntax-rule (check-tuple= t1 t2)
  (unless (and (f= (tuple-x t1) (tuple-x t2))
               (f= (tuple-y t1) (tuple-y t2))
               (f= (tuple-z t1) (tuple-z t2))
               (f= (tuple-w t1) (tuple-w t2)))
    (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define tuples-test
(test-suite "Tuples, Points, and Vectors"
            (test-case "tuple-base"
                       (define a (tuple 4.3 -4.2 3.1 1.0))
                       (check-equal? (tuple-x a) 4.3)
                       (check-equal? (tuple-y a) -4.2)
                       (check-equal? (tuple-z a) 3.1)
                       (check-equal? (tuple-w a) 1.0)
                       (check-true (pt? a))
                       (check-false (vec? a))
                       (define b (tuple 4.3 -4.2 3.1 0.0))
                       (check-equal? (tuple-x b) 4.3)
                       (check-equal? (tuple-y b) -4.2)
                       (check-equal? (tuple-z b) 3.1)
                       (check-equal? (tuple-w b) 0.0)
                       (check-false (pt? b))
                       (check-true (vec? b))
                       (define c (pt 4 -4 3))
                       (check-equal? c (tuple 4 -4 3 1))
                       (define d (vec 4 -4 3))
                       (check-equal? d (tuple 4 -4 3 0)))
            (test-case "addition"
                       (check-tuple= (tuple+ (tuple 3 -2 5 1) (tuple -2 3 1 0)) (tuple 1 1 6 1))
                       (check-tuple= (tuples+ (tuple 3 -2 5 1)) (tuple 3 -2 5 1))
                       (check-tuple= (tuples+ (tuple 3 -2 5 1) (tuple -2 3 1 0)) (tuple 1 1 6 1))
                       (check-tuple= (tuples+ (tuple 3 -2 5 1) (tuple -2 3 1 0) (tuple 1 1 1 1))
                                     (tuple 2 2 7 2)))
            (test-case "subtraction"
                       (check-tuple= (tuple- (pt 3 2 1) (pt 5 6 7)) (vec -2 -4 -6))
                       (check-tuple= (tuples- (tuple 3 -2 5 1)) (tuple 3 -2 5 1))
                       (check-tuple= (tuples- (tuple 3 -2 5 1) (tuple -2 3 1 0)) (tuple 5 -5 4 1))
                       (check-tuple= (tuples- (tuple 3 -2 5 1) (tuple -2 3 1 0) (tuple 1 1 1 1))
                                     (tuple 4 -6 3 0)))
            (test-case "negation"
                       (check-tuple= (tuple- (vec 0 0 0) (vec 1 -2 3)) (vec -1 2 -3))
                       (check-tuple= (-tuple (tuple 1 -2 3 -4)) (tuple -1 2 -3 4)))
            (test-case "multiplication and division"
                       (define a (tuple 1 -2 3 -4))
                       (check-tuple= (tuple* a 3.5) (tuple 3.5 -7.0 10.5 -14.0))
                       (check-tuple= (tuple* a 0.5) (tuple 0.5 -1.0 1.5 -2.0))
                       (check-tuple= (tuple/ (tuple 1.0 -2 3.0 -4) 2) (tuple 0.5 -1 1.5 -2)))
            (test-case "magnitude"
                       (check-= (mag (vec 1 0 0)) 1 0.00001)
                       (check-= (mag (vec 0 1 0)) 1 0.00001)
                       (check-= (mag (vec 0 0 1)) 1 0.00001)
                       (check-= (mag (vec 1 2 3)) (sqrt 14) 0.00001)
                       (check-= (mag (vec -1 -2 -3)) (sqrt 14) 0.00001))
            (test-case "normalization"
                       (check-tuple= (norm (vec 4 0 0)) (vec 1 0 0))
                       (check-tuple= (norm (vec 1 2 3)) (vec 0.26726 0.53452 0.80178)))
            (test-case "dot product" (check-= (dot* (vec 1 2 3) (vec 2 3 4)) 20 0.00001))
            (test-case "cross product"
                       (check-tuple= (cross* (vec 1 2 3) (vec 2 3 4)) (vec -1 2 -1))
                       (check-tuple= (cross* (vec 2 3 4) (vec 1 2 3)) (vec 1 -2 1)))))

(run-tests tuples-test)
