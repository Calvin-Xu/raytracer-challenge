#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "ray.rkt")
(require "intersect.rkt")
(require "material.rkt")
(require "patterns.rkt")
(require "shapes.rkt")
(require "light.rkt")
(require "world.rkt")

(: phong (->* (Material Shape Light Point Vector Vector) (Boolean) Color))
(define (phong material object light point eyev normalv [in-shadow #f])
  (let* ([pattern : Pattern (material-pattern material)]
         [pattern-color : Color
                        (pattern-at pattern object point)]
         [blended : Color
          (color* pattern-color (light-intensity light))]
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
    (if in-shadow
        ambient
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
                     [reflectv : Vector]
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
         [reflectv (reflect (ray-direction ray) adjusted-normalv)]
         [over-pt (assert (tuple+ point (tuple* adjusted-normalv EPSILON)) point?)])
    (intersection-data t object point eyev adjusted-normalv reflectv inside over-pt)))

(: shade-intersection (->* (World IntersectionData) (Exact-Nonnegative-Integer) Color))
(define (shade-intersection world comps [remaining 5])
  (let ([per-light-shading
         :
         (Listof Color)
         (for/list ([light : Light (in-hash-values (world-lights world))])
           (colors+ (phong (shape-material (intersection-data-object comps))
                           (intersection-data-object comps)
                           light
                           (intersection-data-over-pt comps)
                           (intersection-data-eyev comps)
                           (intersection-data-normalv comps)
                           (is-shadowed world light (intersection-data-over-pt comps)))
                    (shade-reflection world comps remaining)))])
    (apply colors+ per-light-shading)))

(: shade-ray (->* (World Ray) (Exact-Nonnegative-Integer) Color))
(define (shade-ray world ray [remaining 5])
  (let* ([intersections : (Listof Intersection) (intersect-world world ray)]
         ;; sorted intersections
         [hit : (U Intersection Null) (fast-hit intersections)])
    (if (null? hit)
        black
        (let* ([precomp : IntersectionData (precomp hit ray)]
               [shade : Color (shade-intersection world precomp remaining)])
          shade))))

(: shade-reflection (-> World IntersectionData Exact-Nonnegative-Integer Color))
(define (shade-reflection world comps remaining)
  (let ([reflective (material-reflective (shape-material (intersection-data-object comps)))])
    (if (or (= 0 reflective) (< remaining 1))
        black
        (let* ([reflect-ray (ray (intersection-data-over-pt comps)
                                 (intersection-data-reflectv comps))]
               [reflect-color (shade-ray world reflect-ray (sub1 remaining))])
          (color* reflect-color reflective)))))
