#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "material.rkt")
(require "ray.rkt")

(struct shape
        ([id : String] [transformation : Matrix]
                       [material : Material]
                       [intersect : (-> Ray (Listof Float))]
                       [normal-at : (-> Point Vector)])
  #:prefab
  #:type-name Shape)

(struct _sphere shape () #:prefab #:type-name Sphere)

(: sphere (->* (String) (#:transformation Matrix #:material Material) Shape))
(define (sphere id #:transformation [transformation id-mat-4] #:material [material (make-material)])
  (: sphere-intersect (-> Ray (Listof Float)))
  (define (sphere-intersect ray)
    (let* ([center-to-ray : Vector (assert (tuple- (ray-origin ray) (pt 0. 0. 0.)) vect?)]
           [a : Float (dot* (ray-direction ray) (ray-direction ray))]
           [b : Float (* 2 (dot* (ray-direction ray) center-to-ray))]
           [c : Float (- (dot* center-to-ray center-to-ray) 1)]
           [discriminant : Float (- (sqr b) (* 4. a c))]
           [solution : (-> (U '+ '-) Float)
                     (lambda (sign)
                       (cast
                        (/ ((if (eq? sign '-) - +) (- b) (sqrt discriminant)) (* 2 a))
                        Float))])
      (if (< discriminant 0.)
          '()
          (list (solution '-) (solution '+)))))
  (: sphere-normal-at (-> Point Vector))
  (define (sphere-normal-at point)
    (assert (tuple- point (pt 0. 0. 0.)) vect?))
  (_sphere id transformation material sphere-intersect sphere-normal-at))

(struct _plane shape () #:prefab #:type-name Plane)

(: plane (->* (String) (#:transformation Matrix #:material Material) Shape))
(define (plane id #:transformation [transformation id-mat-4] #:material [material (make-material)])
  (: plane-intersect (-> Ray (Listof Float)))
  (define (plane-intersect ray)
    (if (< (abs (tuple-y (ray-direction ray))) EPSILON)
        '()
        (list (/ (- (tuple-y (ray-origin ray))) (tuple-y (ray-direction ray))))))
  (: plane-normal-at (-> Point Vector))
  (define (plane-normal-at point)
    (vec 0. 1. 0.))
  (_plane id transformation material plane-intersect plane-normal-at))

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
  (let* ([trans : Matrix (shape-transformation obj)]
         [obj-pt : Point (assert (mat-t* (inverse trans) world-point) point?)]
         [obj-norm : Vector ((shape-normal-at obj) obj-pt)]
         [world-norm : Tuple (mat-t* (transpose (inverse trans)) obj-norm)])
    (norm (vec (tuple-x world-norm) (tuple-y world-norm) (tuple-z world-norm)))))
