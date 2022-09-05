#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "material.rkt")
(require "ray.rkt")
(require "color.rkt")
(require "patterns.rkt")
(require typed/racket/flonum)

(struct shape
        ([id : String] [transformation : Matrix]
                       [material : Material]
                       [intersect : (-> Ray (Listof Float))]
                       [normal-at : (-> Point Vector)]
                       [inv-trans : Matrix])
  #:prefab
  #:type-name Shape)

(struct _sphere shape () #:prefab #:type-name Sphere)

(: sphere (->* (String) (#:transformation Matrix #:material Material) Shape))
(define (sphere id #:transformation [transformation id-mat-4] #:material [material (make-material)])
  (: sphere-intersect (-> Ray (Listof Float)))
  (define (sphere-intersect ray)
    (let* ([center-to-ray : Vector (assert (tuple- (ray-origin ray) (pt 0. 0. 0.)) vect?)]
           [a : Float (dot* (ray-direction ray) (ray-direction ray))]
           [b : Float (fl* 2. (dot* (ray-direction ray) center-to-ray))]
           [c : Float (fl- (dot* center-to-ray center-to-ray) 1.)]
           [discriminant : Float (fl- (sqr b) (* 4. a c))]
           [solution : (-> (U '+ '-) Float)
                     (lambda (sign)
                        (/ ((if (eq? sign '-) fl- fl+) (- b) (flsqrt discriminant)) (* 2 a)))])
      (if (< discriminant 0.)
          '()
          (list (solution '-) (solution '+)))))
  (: sphere-normal-at (-> Point Vector))
  (define (sphere-normal-at point)
    (assert (tuple- point (pt 0. 0. 0.)) vect?))
  (_sphere id transformation material sphere-intersect sphere-normal-at (inverse transformation)))

(struct _plane shape () #:prefab #:type-name Plane)

(: plane (->* (String) (#:transformation Matrix #:material Material) Shape))
(define (plane id #:transformation [transformation id-mat-4] #:material [material (make-material)])
  (: plane-intersect (-> Ray (Listof Float)))
  (define (plane-intersect ray)
    (if (fl< (abs (tuple-y (ray-direction ray))) EPSILON)
        '()
        (list (fl/ (- (tuple-y (ray-origin ray))) (tuple-y (ray-direction ray))))))
  (: plane-normal-at (-> Point Vector))
  (define (plane-normal-at point)
    (vec 0. 1. 0.))
  (_plane id transformation material plane-intersect plane-normal-at (inverse transformation)))

(: set-transformation
   (-> (->* (String) (#:transformation Matrix #:material Material) Shape) Shape Matrix Shape))
(define (set-transformation constructor val trans)
  (constructor (shape-id val) #:transformation trans #:material (shape-material val)))

(: set-material
   (-> (->* (String) (#:transformation Matrix #:material Material) Shape) Shape Material Shape))
(define (set-material constructor val material)
  (constructor (shape-id val) #:transformation (shape-transformation val) #:material material))

(: normal-at (-> Shape Point Vector))
(define (normal-at obj world-point)
  (let* ([inv-trans : Matrix (shape-inv-trans obj)]
         [obj-pt : Point (assert (mat-t* inv-trans world-point) point?)]
         [obj-norm : Vector ((shape-normal-at obj) obj-pt)]
         [world-norm : Tuple (mat-t* (transpose inv-trans) obj-norm)])
    (norm (vec (tuple-x world-norm) (tuple-y world-norm) (tuple-z world-norm)))))

(: pattern-at (-> Pattern Shape Point Color))
(define (pattern-at pattern object point)
  (let* ([obj-pt (mat-t* (shape-inv-trans object) point)]
         [pattern-pt (assert (mat-t* (pattern-inv-trans pattern) obj-pt) point?)])
    ((_pattern-color-at pattern) pattern-pt)))
