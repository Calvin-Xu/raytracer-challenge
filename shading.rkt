#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "ray.rkt")
(require "intersect.rkt")
(require "material.rkt")
(require "shapes.rkt")
(require "light.rkt")
(require "world.rkt")

(: phong (->* (Material Light Point Vector Vector) (Boolean) Color))
(define (phong material light point eyev normalv [in-shadow #f])
  (if in-shadow
      (let ([ambient (material-ambient material)])
        (color ambient ambient ambient))
  (let* ([blended : Color
          (color* (material-color material) (light-intensity light))]
         [ambient : Color
          (color* blended (material-ambient material))]
         [lightv : Vector
          (norm (assert (tuple- (light-position light) point) vect?))]
         [*light-normal : Float (dot* lightv normalv)]
         [diffuse : Color
          (if (< *light-normal 0.)
              black
              (color* blended (* (material-diffuse material) *light-normal)))]
         [specular : Color
          (if (< *light-normal 0.)
              black
              (let* ([reflectv : Vector
                      (reflect (assert (-tuple lightv) vect?) normalv)]
                     [*reflect-eye : Float (dot* reflectv eyev)])
                (if (< *reflect-eye 0.)
                    black
                    (color* (light-intensity light)
                            (* (material-specular material)
                               (cast (expt *reflect-eye (material-shininess material)) Float))))))])
    (colors+ ambient diffuse specular))))

(: is-shadowed (-> World Light Point Boolean))
(define (is-shadowed world light point)
         (let* ([v : Vector (assert (tuple- (light-position light) point) vect?)]
                [distance : Float (mag v)]
                [direction : Vector (norm v)]
                [r : Ray (ray point direction)]
                [intersections : (Listof Intersection) (intersect-world world r)]
                [hit : (U Null Intersection) (hit intersections)])
           (cond
             [(null? hit) #f]
             [(< (intersection-t hit) distance) #t]
             [else #f])))

(struct intersection-data
        ([t : Float] [object : Shape]
                     [point : Point]
                     [eyev : Vector]
                     [normalv : Vector]
                     [inside : Boolean]
                     [over-pt : Point])
  #:prefab
  #:type-name IntersectionData)

(: precomp (-> Intersection Ray IntersectionData))
(define (precomp intersection ray)
  (let* ([t (intersection-t intersection)]
         [object (intersection-obj intersection)]
         [point (pos ray t)]
         [eyev (assert (-tuple (ray-direction ray)) vect?)]
         [normalv (normal-at object point)]
         [-normalv (assert (-tuple normalv) vect?)]
         [inside (if (< (dot* normalv eyev) 0.) #t #f)]
         [adjusted-normalv (if inside -normalv normalv)]
         [over-pt (assert (tuple+ point (tuple* adjusted-normalv EPSILON)) point?)])
    (intersection-data t object point eyev adjusted-normalv inside over-pt)))

(: shade-intersection (-> World IntersectionData Color))
(define (shade-intersection world comps)
  (let ([per-light-shading : (Listof Color)
         (for/list ([light : Light (in-hash-values (world-lights world))])
           (phong (shape-material (intersection-data-object comps))
                     light
                     (intersection-data-point comps)
                     (intersection-data-eyev comps)
                     (intersection-data-normalv comps)
                     (is-shadowed world light (intersection-data-over-pt comps))))])
    (apply colors+ per-light-shading)))

(: shade-ray (-> World Ray Color))
(define (shade-ray world ray)
  (let* ([intersections : (Listof Intersection) (intersect-world world ray)]
         ;; sorted intersections
         [hit : (U Intersection Null) (fast-hit intersections)])
    (if (null? hit)
        black
        (let* ([precomp : IntersectionData (precomp hit ray)]
               [shade : Color (shade-intersection world precomp)])
          shade))))
