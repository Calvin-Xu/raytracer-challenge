#lang typed/racket
(provide (all-defined-out))
(require "matrix.rkt")
(require "ray.rkt")
(require "shapes.rkt")
(require "world.rkt")

(struct intersection ([t : Float] [obj : Shape]) #:prefab #:type-name Intersection)

(: intersect (-> Shape Ray (Listof Intersection)))
(define (intersect shape ray)
  (let ([local-ray : Ray
                   (transform-ray ray (shape-inv-trans shape))])
    (map (lambda ([t : Float]) (intersection t shape)) ((shape-intersect shape) local-ray))))

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
