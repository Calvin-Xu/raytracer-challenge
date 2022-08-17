#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "matrix.rkt")
(require "transform.rkt")
(require "ray.rkt")
(require "shapes.rkt")
(require "world.rkt")
(require "lighting.rkt")

(struct intersection ([t : Float] [obj : Shape]) #:prefab #:type-name Intersection)

(: intersect (-> Shape Ray (Listof Intersection)))
(define (intersect shape ray)
  (let* ([ray : Ray (transform-ray ray (inverse (shape-transformation shape)))]
         [center-to-ray : Vector (assert (tuple- (ray-origin ray) (pt 0. 0. 0.)) vect?)]
         [a : Float (dot* (ray-direction ray) (ray-direction ray))]
         [b : Float (* 2 (dot* (ray-direction ray) center-to-ray))]
         [c : Float (- (dot* center-to-ray center-to-ray) 1)]
         [discriminant : Float (- (sqr b) (* 4. a c))]
         [solution : (-> (U '+ '-) Float)
          (lambda (sign)
            (cast (/ ((if (eq? sign '-) - +) (- b) (sqrt discriminant)) (* 2 a)) Float))])
    (if (< discriminant 0.)
        '()
        (list (intersection (solution '-) shape) (intersection (solution '+) shape)))))

(: hit (-> (Listof Intersection) (U Intersection Null)))
(define (hit intersections)
  (: iter (-> (Listof Intersection) Intersection (U Intersection Null)))
  (define (iter remaining result)
    (if (null? remaining)
        (if (= (intersection-t result) +inf.0) null result)
        (iter (cdr remaining)
              (if (and (>= (intersection-t (car remaining)) 0.)
                       (< (intersection-t (car remaining)) (intersection-t result)))
                  (car remaining)
                  result))))
  (iter intersections (intersection +inf.0 (sphere "placeholder"))))

(: intersect-world (-> World Ray (Listof Intersection)))
(define (intersect-world world ray)
  (for/fold ([intersections : (Listof Intersection) '()]
             #:result ((inst sort Intersection) intersections #:key intersection-t <=))
            ([obj (in-hash-values (world-objects world))])
    (append intersections (intersect obj ray))))

(struct intersection-data
        ([t : Float] [object : Shape]
                     [point : Point]
                     [eyev : Vector]
                     [normalv : Vector]
                     [inside : Boolean])
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
         [inside (if (< (dot* normalv eyev) 0.) #t #f)])
    (intersection-data t object point eyev (if inside -normalv normalv) inside)))

(: shade (-> World IntersectionData Color))
(define (shade world comps)
  (let ([per-light-shading : (Listof Color)
         (for/list ([light : Light (in-hash-values (world-lights world))])
           (lighting (shape-material (intersection-data-object comps))
                     light
                     (intersection-data-point comps)
                     (intersection-data-eyev comps)
                     (intersection-data-normalv comps)))])
    (apply colors+ per-light-shading)))

(: shade-ray (-> World Ray Color))
(define (shade-ray world ray)
  (let* ([intersections : (Listof Intersection) (intersect-world world ray)]
         [hit : (U Intersection Null) (hit intersections)])
    (if (null? hit)
        black
        (let* ([precomp : IntersectionData (precomp hit ray)]
               [shade : Color (shade world precomp)])
          shade))))
