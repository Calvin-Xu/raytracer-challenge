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
    (check-equal? (mat-m m) 4)
    (check-equal? (mat-n m) 4)
    (check-equal? (mat-entry m 0 0) 1.)
    (check-equal? (mat-entry m 0 3) 4.)
    (check-equal? (mat-entry m 1 0) 5.5)
    (check-equal? (mat-entry m 1 2) 7.5)
    (check-equal? (mat-entry m 2 2) 11.)
    (check-equal? (mat-entry m 3 0) 13.5)
    (check-equal? (mat-entry m 3 2) 15.5)
    (define m2 (mat 2 2 #[#[-3. 5.] #[1. -2.]]))
    (check-equal? (mat-m m2) 2)
    (check-equal? (mat-n m2) 2)
    (check-equal? (mat-entry m2 0 0) -3.)
    (check-equal? (mat-entry m2 0 1) 5.)
    (check-equal? (mat-entry m2 1 0) 1.)
    (check-equal? (mat-entry m2 1 1) -2.)
    (define m3 (mat 3 3 #[#[-3. 5. 0.] #[1. -2. -7.] #[0. 1. 1.]]))
    (check-equal? (mat-m m3) 3)
    (check-equal? (mat-n m3) 3)
    (check-equal? (mat-entry m3 0 0) -3.)
    (check-equal? (mat-entry m3 1 1) -2.)
    (check-equal? (mat-entry m3 2 2) 1.))
   (test-case "compare matrix"
              (define a (mat 4 4 #[#[1. 2. 3. 4.] #[5. 6. 7. 8.] #[9. 8. 7. 6.] #[5. 4. 3. 2.]]))
              (define b (mat 4 4 #[#[1. 2. 3. 4.] #[5. 6. 7. 8.] #[9. 8. 7. 6.] #[5. 4. 3. 2.]]))
              (check-true (mat= a b))
              (define c (mat 4 4 #[#[2. 3. 4. 5.] #[6. 7. 8. 9.] #[8. 7. 6. 5.] #[4. 3. 2. 1.]]))
              (check-false (mat= a c)))
   (test-case "get matrix row and column"
              (define a (mat 4 4 #[#[1. 2. 3. 4.] #[5. 6. 7. 8.] #[9. 8. 7. 6.] #[5. 4. 3. 2.]]))
              (check-equal? (mat-row a 2) #[9. 8. 7. 6.])
              (check-equal? (mat-col a 1) #[2. 6. 8. 4.]))
   (test-case
    "multiply matrices"
    (define a (mat 4 4 #[#[1. 2. 3. 4.] #[5. 6. 7. 8.] #[9. 8. 7. 6.] #[5. 4. 3. 2.]]))
    (define b (mat 4 4 #[#[-2. 1. 2. 3.] #[3. 2. 1. -1.] #[4. 3. 6. 5.] #[1. 2. 7. 8.]]))
    (define c
      (mat 4 4 #[#[20. 22. 50. 48.] #[44. 54. 114. 108.] #[40. 58. 110. 102.] #[16. 26. 46. 42.]]))
    (check-true (mat= (mat* a b) c)))
   (test-case "multiply matrix by vector (tuple)"
              (define a (mat 4 4 #[#[1. 2. 3. 4.] #[2. 4. 4. 2.] #[8. 6. 4. 1.] #[0. 0. 0. 1.]]))
              (define b (tuple 1. 2. 3. 1.))
              (check-tuple= (mat-tuple* a b) (tuple 18. 24. 33. 1.)))
   (test-case "multiply identity matrix"
              (define a (mat 4 4 #[#[0. 1. 2. 4.] #[1. 2. 4. 8.] #[2. 4. 8. 16.] #[4. 8. 16. 32.]]))
              (check-true (mat= (mat* a id-mat-4) a))
              (define b (tuple 1. 2. 3. 4.))
              (check-tuple= (mat-tuple* id-mat-4 b) b))
   (test-case "transpose matrices"
     (define a (mat 4 4 #[#[0. 9. 3. 0.] #[9. 8. 0. 8.] #[1. 8. 5. 3.] #[0. 0. 5. 8.]]))
     (define b (mat 4 4 #[#[0. 9. 1. 0.] #[9. 8. 8. 0.] #[3. 0. 5. 5.] #[0. 8. 3. 8.]]))
     (check-true (mat= (mat-T a) b))
     (check-true (mat= (mat-T b) a))
     (check-true (mat= (mat-T id-mat-4) id-mat-4)))))

(run-tests canvas-test)
