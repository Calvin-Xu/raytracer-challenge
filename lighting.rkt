#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "matrix.rkt")
(require "material.rkt")
(require "shapes.rkt")

(struct light ([position : Point] [intensity : Color]) #:prefab #:type-name Light)
(struct point-light light () #:prefab #:type-name PointLight)

(: reflect (-> Vector Vector Vector))
(define (reflect in normal)
  (assert (tuple- in (tuple* normal (* 2 (dot* in normal)))) vect?))

(: normal-at (-> Shape Point Vector))
(define (normal-at sphere world-point)
  (let* ([trans : Matrix (shape-transformation sphere)]
         [obj-pt : Point (assert (mat-t* (inverse trans) world-point) point?)]
         [obj-norm : Vector (assert (tuple- obj-pt (pt 0. 0. 0.)) vect?)]
         [world-norm : Tuple (mat-t* (transpose (inverse trans)) obj-norm)])
    (norm (vec (tuple-x world-norm) (tuple-y world-norm) (tuple-z world-norm)))))

(: lighting (-> Material PointLight Point Vector Vector Color))
(define (lighting material light point eyev normalv)
  (let* ([blended
          :
          Color
          (color* (material-color material) (light-intensity light))]
         [ambient
          :
          Color
          (color* blended (material-ambient material))]
         [lightv
          :
          Vector
          (norm (assert (tuple- (light-position light) point) vect?))]
         [*light-normal
          :
          Float
          (dot* lightv normalv)]
         [diffuse
          :
          Color
          (if (< *light-normal 0)
              black
              (color* blended (* (material-diffuse material) *light-normal)))]
         [specular
          :
          Color
          (if (< *light-normal 0)
              black
              (let* ([reflectv
                      :
                      Vector
                      (reflect (assert (-tuple lightv) vect?) normalv)]
                     [*reflect-eye
                      :
                      Float
                      (dot* reflectv eyev)])
                (if (< *reflect-eye 0)
                    black
                    (color* (light-intensity light)
                            (* (material-specular material)
                               (expt *reflect-eye (material-shininess material)))))))])
    (colors+ ambient diffuse specular)))
