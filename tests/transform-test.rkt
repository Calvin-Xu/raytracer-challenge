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
  (test-suite
   "Transformations"
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
                          (check-tuple= (mat-t* t p) (pt -2. 3. 4.))))
   (test-suite
    "rotation"
    (test-case "Rotating a point around the x axis"
               (define p (pt 0. 1. 0.))
               (define half-quarter (rotate 'x (/ pi 4)))
               (define full-quarter (rotate 'x (/ pi 2)))
               (check-tuple= (mat-t* half-quarter p)
                             (pt 0. (cast (/ (sqrt 2) 2) Float) (cast (/ (sqrt 2) 2) Float)))
               (check-tuple= (mat-t* full-quarter p) (pt 0. 0. 1.)))
    (test-case "The inverse of an x-rotation rotates in the opposite direction"
               (define p (pt 0. 1. 0.))
               (define half-quarter (rotate 'x (/ pi 4)))
               (define inv (inverse half-quarter))
               (check-tuple= (mat-t* inv p)
                             (pt 0. (cast (/ (sqrt 2) 2) Float) (cast (- (/ (sqrt 2) 2)) Float))))
    (test-case "Rotating a point around the y axis"
               (define p (pt 0. 0. 1.))
               (define half-quarter (rotate 'y (/ pi 4)))
               (define full-quarter (rotate 'y (/ pi 2)))
               (check-tuple= (mat-t* half-quarter p)
                             (pt (cast (/ (sqrt 2) 2) Float) 0. (cast (/ (sqrt 2) 2) Float)))
               (check-tuple= (mat-t* full-quarter p) (pt 1. 0. 0.)))
    (test-case "Rotating a point around the z axis"
               (define p (pt 0. 1. 0.))
               (define half-quarter (rotate 'z (/ pi 4)))
               (define full-quarter (rotate 'z (/ pi 2)))
               (check-tuple= (mat-t* half-quarter p)
                             (pt (cast (- (/ (sqrt 2) 2)) Float) (cast (/ (sqrt 2) 2) Float) 0.))
               (check-tuple= (mat-t* full-quarter p) (pt -1. 0. 0.))))
   (test-suite "shearing"
               (test-case "A shearing transformation moves x in proportion to y"
                          (define t (shear 1. 0. 0. 0. 0. 0.))
                          (define p (pt 2. 3. 4.))
                          (check-tuple= (mat-t* t p) (pt 5. 3. 4.)))
               (test-case "A shearing transformation moves x in proportion to z"
                          (define t (shear 0. 1. 0. 0. 0. 0.))
                          (define p (pt 2. 3. 4.))
                          (check-tuple= (mat-t* t p) (pt 6. 3. 4.)))
               (test-case "A shearing transformation moves y in proportion to x"
                          (define t (shear 0. 0. 1. 0. 0. 0.))
                          (define p (pt 2. 3. 4.))
                          (check-tuple= (mat-t* t p) (pt 2. 5. 4.)))
               (test-case "A shearing transformation moves y in proportion to z"
                          (define t (shear 0. 0. 0. 1. 0. 0.))
                          (define p (pt 2. 3. 4.))
                          (check-tuple= (mat-t* t p) (pt 2. 7. 4.)))
               (test-case "A shearing transformation moves z in proportion to x"
                          (define t (shear 0. 0. 0. 0. 1. 0.))
                          (define p (pt 2. 3. 4.))
                          (check-tuple= (mat-t* t p) (pt 2. 3. 6.)))
               (test-case "A shearing transformation moves z in proportion to y"
                          (define t (shear 0. 0. 0. 0. 0. 1.))
                          (define p (pt 2. 3. 4.))
                          (check-tuple= (mat-t* t p) (pt 2. 3. 7.))))))

(run-tests transform-test)
