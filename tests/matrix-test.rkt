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

(define matrix-test
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

(define matrix-inversion-test
  (test-suite
   "Matrix Inversion"
   (test-case "find determinant of 2x2 matrix"
              (define m (mat 2 2 #[#[1. 5.] #[-3. 2.]]))
              (check-equal? (det-2 m) 17.))
   ;; (test-case "find submatrices"
   ;;            (define a (mat 3 3 #[#[1. 5. 0.] #[-3. 2. 7.] #[0. 6. -3.]]))
   ;;            (define b (mat 2 2 #[#[-3. 2.] #[0. 6.]]))
   ;;            (check-true (mat= (mat-sub a 0 2) b))
   ;;            (define c (mat 4 4 #[#[-6. 1. 1. 6.] #[-8. 5. 8. 6.] #[-1. 0. 8. 2.] #[-7. 1. -1. 1.]]))
   ;;            (define d (mat 3 3 #[#[-6. 1. 6.] #[-8. 8. 6.] #[-7. -1. 1.]]))
   ;;            (check-true (mat= (mat-sub c 2 1) d)))
   ;; (test-case "find minor"
   ;;            (define a (mat 3 3 #[#[3. 5. 0.] #[2. -1. -7.] #[6. -1. 5.]]))
   ;;            (define b (mat-sub a 1 0))
   ;;            (check-equal? (det-2 b) 25.)
   ;;            (check-equal? (minor a 1 0) 25.))
   ;; (test-case "find cofactor"
   ;;            (define a (mat 3 3 #[#[3. 5. 0.] #[2. -1. -7.] #[6. -1. 5.]]))
   ;;            (check-equal? (minor a 0 0) -12.)
   ;;            (check-equal? (cofactor a 0 0) -12.)
   ;;            (check-equal? (minor a 1 0) 25.)
   ;;            (check-equal? (cofactor a 1 0) -25.))
   ;; (test-case
   ;;  "find determinant of larger matrices"
   ;;  (define a (mat 3 3 #[#[1. 2. 6.] #[-5. 8. -4] #[2. 6. 4.]]))
   ;;  (check-equal? (cofactor a 0 0) 56.)
   ;;  (check-equal? (cofactor a 0 1) 12.)
   ;;  (check-equal? (cofactor a 0 2) -46.)
   ;;  (check-equal? (det a) -196.)
   ;;  (define b (mat 4 4 #[#[-2. -8. 3. 5.] #[-3. 1. 7. 3.] #[1. 2. -9. 6.] #[-6. 7. 7. -9.]]))
   ;;  (check-equal? (cofactor b 0 0) 690.)
   ;;  (check-equal? (cofactor b 0 1) 447.)
   ;;  (check-equal? (cofactor b 0 2) 210.)
   ;;  (check-equal? (cofactor b 0 3) 51.)
   ;;  (check-equal? (det a) -4071.)
   ;;  (define c (mat 4 4 #[#[6. 4. 4. 4.] #[5. 5. 7. 6.] #[4. -9. 3. -7.] #[9. 1. 7. 6.]]))
   ;;  (check-equal? (det c) -2120.)
   ;;  (define d (mat 4 4 #[#[-4. 2. -2. -3.] #[9. 6. 2. 6.] #[0. -5. 1. -5.] #[0. 0. 0. 0.]]))
   ;;  (check-equal? (det d) 0.))
   ;; (test-case
   ;;  "inverse matrix test 1"
   ;;  (define a (mat 4 4 #[#[-5. 2. 6. -8.] #[1. -5. 1. 8.] #[7. 7. -6. -7.] #[1. -3. 7. 4.]]))
   ;;  (define b (inverse a))
   ;;  (check-equal? (det a) 532.)
   ;;  (check-equal? (cofactor a 2 3) -160.)
   ;;  (check-= (mat-entry b 3 2) -160/532 0.00001)
   ;;  (check-equal? (cofactor a 3 2) 105.)
   ;;  (check-= (mat-entry b 2 3) 105/532 0.00001)
   ;;  (define c
   ;;    (mat 4
   ;;         4
   ;;         (#[#[0.21805 0.45113 0.24060 -0.04511]
   ;;            #[-0.80827 -1.45677 -0.44361 0.52068]
   ;;            #[-0.07895 -0.22368 -0.05263 0.19737]
   ;;            #[-0.52256 -0.81391 -0.30075 0.30639]])))
   ;;  (check-true (mat= b c)))
   ;; (test-case "inverse matrix test 2"
   ;;            (define a (mat 4 4 #[8. -5. 9. 2.] #[7. 5. 6. 1.] #[-6. 0. 9. 6.] #[-3. 0. -9. -4.]))
   ;;            (define b
   ;;              (mat 4
   ;;                   4
   ;;                   #[#[-0.15385 -0.15385 -0.28205 -0.53846]
   ;;                     #[-0.07692 0.12308 0.02564 0.03077]
   ;;                     #[0.35897 0.35897 0.43590 0.92308]
   ;;                     #[-0.69231 -0.69231 -0.76923 -1.92308]]))
   ;;            (check-true (mat= (inverse a) b)))
   ;; (test-case "inverse matrix test 3"
   ;;            (define a (mat 4 4 #[9. 3. 0. 9.] #[-5. -2. -6. -3.] #[-4. 9. 6. 4.] #[-7. 6. 6. 2.]))
   ;;            (define b
   ;;              (mat 4
   ;;                   4
   ;;                   #[#[-0.04074 -0.07778 0.14444 -0.22222]
   ;;                     #[-0.07778 0.03333 0.36667 -0.33333]
   ;;                     #[-0.02901 -0.14630 -0.10926 0.12963]
   ;;                     #[0.17778 0.06667 -0.26667 0.33333]]))
   ;;            (check-true (mat= (inverse a) b)))
   ;; (test-case "multiply matrix product by inverse"
   ;;            (define a (mat 4 4 #[#[3. -9. 7. 3.] #[3. -8. 2. -9.] #[-4. 4. 4. 1.] #[-6. 5. -1. 1]]))
   ;;            (define b (mat 4 4 #[#[8. 2. 2. 2.] #[3. -1. 7. 0.] #[7. 0. 5. 4.] #[6. -2. 0. 5.]]))
   ;;            (define c (mat* a b))
   ;;            (define d (mat* c (inverse b)))
   ;;            (check-true (mat= a d)))
   ))

(run-tests matrix-test)
(run-tests matrix-inversion-test)
