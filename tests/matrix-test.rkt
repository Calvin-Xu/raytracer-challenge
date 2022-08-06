#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../matrix.rkt")

(define-syntax-rule (check-tuple= t1 t2)
  (unless (and (f= (tuple-x t1) (tuple-x t2))
               (f= (tuple-y t1) (tuple-y t2))
               (f= (tuple-z t1) (tuple-z t2))
               (f= (tuple-w t1) (tuple-w t2)))
    (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define canvas-test
  (test-suite
   "Matrices"
   (test-case
    "define matrix"
    (define m (mat 4 4 #[#[1. 2. 3. 4.] #[5.5 6.5 7.5 8.5] #[9. 10. 11. 12.] #[13.5 14.5 15.5 16.5]]))
    (check-equal? (mat-entry m 0 0) 1.)
    (check-equal? (mat-entry m 0 3) 4.)
    (check-equal? (mat-entry m 1 0) 5.5)
    (check-equal? (mat-entry m 1 2) 7.5)
    (check-equal? (mat-entry m 2 2) 11.)
    (check-equal? (mat-entry m 3 0) 13.5)
    (check-equal? (mat-entry m 3 2) 15.5)
    (define m2 (mat 2 2 #[#[-3. 5.] #[1. -2.]]))
    (check-equal? (mat-entry m2 0 0) -3.)
    (check-equal? (mat-entry m2 0 1) 5.)
    (check-equal? (mat-entry m2 1 0) 1.)
    (check-equal? (mat-entry m2 1 1) -2.)
    (define m3 (mat 3 3 #[#[-3. 5. 0.] #[1. -2. -7.] #[0. 1. 1.]]))
    (check-equal? (mat-entry m3 0 0) -3.)
    (check-equal? (mat-entry m3 1 1) -2.)
    (check-equal? (mat-entry m3 2 2) 1.))
   (test-case "compare matrix"
              (define a (mat 4 4 #[#[1. 2. 3. 4.] #[5. 6. 7. 8.] #[9. 8. 7. 6.] #[5. 4. 3. 2.]]))
              (define b (mat 4 4 #[#[1. 2. 3. 4.] #[5. 6. 7. 8.] #[9. 8. 7. 6.] #[5. 4. 3. 2.]]))
              (check-true (mat= a b))
              (define c (mat 4 4 #[#[2. 3. 4. 5.] #[6. 7. 8. 9.] #[8. 7. 6. 5.] #[4. 3. 2. 1.]]))
              (check-false (mat= a c)))))

(run-tests canvas-test)
