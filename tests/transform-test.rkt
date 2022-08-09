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
  (test-suite "Transformations"
              (test-suite "Translation"
                          (test-case "Multiplying by a translation matrix"
                                     (define t (translate 5. -3. 2.))
                                     (define p (pt -3. 4. 5.))
                                     (check-tuple= (mat-t* t p) (pt 2. 1. 7.)))
                          (test-case "Multiplying by the inverse of a translate matrix"
                                     (define t (translate 5. -3. 2.))
                                     (define inv (inverse t))
                                     (define p (pt -3. 4. 5.))
                                     (check-tuple= (mat-t* inv p) (pt -8. 7. 3.)))
                          (test-case "Translation does not affect vectors"
                                     (define t (translate 5. -3. 2.))
                                     (define v (vec -3. 4. 5.))
                                     (check-tuple= (mat-t* t v) v)))
              (test-suite "scaling"
                          (test-case "A scaling matrix applied to a point"
                                     (define t (scale 2. 3. 4.))
                                     (define p (pt -4. 6. 8.))
                                     (check-tuple= (mat-t* t p) (pt -8. 18. 32.)))
                          (test-case "A scaling matrix applied to a vector"
                                     (define t (scale 2. 3. 4.))
                                     (define v (vec -4. 6. 8.))
                                     (check-tuple= (mat-t* t v) (vec -8. 18. 32.)))
                          (test-case "Multiplying by the inverse of a scaling matrix"
                                     (define t (scale 2. 3. 4.))
                                     (define inv (inverse t))
                                     (define v (vec -4. 6. 8.))
                                     (check-tuple= (mat-t* inv v) (vec -2. 2. 2.)))
                          (test-case "Reflection is scaling by a negative value"
                                     (define t (scale -1. 1. 1.))
                                     (define p (pt 2. 3. 4.))
                                     (check-tuple= (mat-t* t p) (pt -2. 3. 4.))))))

(run-tests transform-test)
