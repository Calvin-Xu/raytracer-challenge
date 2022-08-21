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
  (define (sphere-normal-at obj-pt)
    (assert (tuple- obj-pt (pt 0. 0. 0.)) vect?))
  (_sphere id transformation material sphere-intersect sphere-normal-at))

(: set-transformation
   (-> (->* (String) (#:transformation Matrix #:material Material) Shape) Shape Matrix Shape))
(define (set-transformation constructor val trans)
  (constructor (shape-id val) #:transformation trans #:material (shape-material val)))

(: set-material
   (-> (->* (String) (#:transformation Matrix #:material Material) Shape) Shape Material Shape))
(define (set-material constructor val material)
  (constructor (shape-id val) #:transformation (shape-transformation val) #:material material))
