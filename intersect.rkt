#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")
(require "matrix.rkt")
(require "transform.rkt")
(require "ray.rkt")
(require "shapes.rkt")
(require "world.rkt")

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

(: fast-hit (-> (Listof Intersection) (U Intersection Null)))
  (define (fast-hit intersections)
    (cond
      [(null? intersections) null]
      [(>= (intersection-t (car intersections)) 0.) (car intersections)]
      [else (fast-hit (cdr intersections))]))

(: intersect-world (-> World Ray (Listof Intersection)))
(define (intersect-world world ray)
  (for/fold ([intersections : (Listof Intersection) '()]
             #:result ((inst sort Intersection) intersections #:key intersection-t <=))
            ([obj (in-hash-values (world-objects world))])
    (append intersections (intersect obj ray))))
