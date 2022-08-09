#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../matrix.rkt"
         "../transform.rkt")

(define-syntax-rule (check-tuple= t1 t2)
  (unless (and (f= (tuple-x t1) (tuple-x t2))
               (f= (tuple-y t1) (tuple-y t2))
               (f= (tuple-z t1) (tuple-z t2))
               (f= (tuple-w t1) (tuple-w t2)))
    (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define transform-test
  (test-suite "Translation"
              (test-case "Multiplying by a translation matrix"
                         (define t (translation 5. -3. 2.))
                         (define p (pt -3. 4. 5.))
                         (check-tuple= (mat-t* t p) (pt 2. 1. 7.)))
              (test-case "Multiplying by the inverse of a translation matrix"
                         (define t (translation 5. -3. 2.))
                         (define inv (inverse t))
                         (define p (pt -3. 4. 5.))
                         (check-tuple= (mat-t* inv p) (pt -8. 7. 3.)))
              (test-case "Translation does not affect vectors"
                         (define t (translation 5. -3. 2.))
                         (define v (vec -3. 4. 5.))
                         (check-tuple= (mat-t* t v) v))))

(run-tests transform-test)
