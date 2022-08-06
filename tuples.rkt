#lang typed/racket

(struct tuple ([x : Float] [y : Float] [z : Float] [w : Float]) #:prefab #:type-name Tuple)
(struct point tuple () #:prefab #:type-name Point)
(struct vect tuple () #:prefab #:type-name Vector)

(: pt (->* (Float Float Float) (Float) Point))
(define (pt x y z [w 1.])
  (point x y z w))

(: pt? (-> Tuple Boolean))
(define (pt? t)
  (= (tuple-w t) 1.))

(: vec (->* (Float Float Float) (Float) Vector))
(define (vec x y z [w 0.])
  (vect x y z w))

(: vec? (-> Tuple Boolean))
(define (vec? t)
  (= (tuple-w t) 0.))

(: f= (-> Float Float Boolean))
(define (f= a b)
  (: EPSILON Float)
  (define EPSILON 0.00001)
  (< (abs (- a b)) EPSILON))

(: tuple+ (-> Tuple Tuple Tuple))
(define (tuple+ t1 t2)
  (let* ([xyzw : (List Float Float Float Float)
          (list (+ (tuple-x t1) (tuple-x t2))
                (+ (tuple-y t1) (tuple-y t2))
                (+ (tuple-z t1) (tuple-z t2))
                (+ (tuple-w t1) (tuple-w t2)))]
         [xyz : (List Float Float Float)
          (reverse (cdr (reverse xyzw)))])

    (cond
      [(and (pt? t1) (pt? t2) (error "Illegal operation: point + point" t1 t2))]
      [(or (and (pt? t1) (vec? t2)) (and (pt? t2) (vec? t1))) (apply pt xyz)]
      [(and (vec? t1) (vec? t2)) (apply vec xyz)]
      [else (apply tuple xyzw)])))

(: tuples+ (-> Tuple * Tuple))
(define (tuples+ . tuples)
  (foldl tuple+ (tuple 0. 0. 0. 0.) tuples))

(: tuple- (-> Tuple Tuple Tuple))
(define (tuple- t1 t2)
  (let* ([xyzw : (List Float Float Float Float)
          (list (- (tuple-x t1) (tuple-x t2))
                (- (tuple-y t1) (tuple-y t2))
                (- (tuple-z t1) (tuple-z t2))
                (- (tuple-w t1) (tuple-w t2)))]
         [xyz : (List Float Float Float)
          (reverse (cdr (reverse xyzw)))])

    (cond
      [(and (vec? t1) (pt? t2) (error "Illegal operation: vector - point" t1 t2))]
      [(and (pt? t1) (pt? t2)) (apply vec xyz)]
      [(and (vec? t1) (vec? t2)) (apply vec xyz)]
      [(and (pt? t1) (vec? t2)) (apply pt xyz)]
      [else (apply tuple xyzw)])))

(: tuples- (-> Tuple * Tuple))
(define (tuples- . tuples)
  ;; optimization when the list is known non-empty
  (if (null? tuples)
      (error "Illegal operation: no arguments provided")
      (foldl tuple+ (car tuples) (map -tuple (cdr tuples)))))

(: -tuple (-> Tuple Tuple))
(define (-tuple t)
  (tuple (- (tuple-x t)) (- (tuple-y t)) (- (tuple-z t)) (- (tuple-w t))))

(: tuple* (-> Tuple Float Tuple))
(define (tuple* t s)
  (tuple (* (tuple-x t) s) (* (tuple-y t) s) (* (tuple-z t) s) (* (tuple-w t) s)))

(: tuple/ (-> Tuple Float Tuple))
(define (tuple/ t s)
  (tuple (/ (tuple-x t) s) (/ (tuple-y t) s) (/ (tuple-z t) s) (/ (tuple-w t) s)))

(: mag (-> Vector Float))
(define (mag v)
  (sqrt (+ (sqr (tuple-x v)) (sqr (tuple-y v)) (sqr (tuple-z v)))))

(: norm (-> Vector Vector))
(define (norm v)
  (let ([mag : Float (mag v)])
    (vec (/ (tuple-x v) mag) (/ (tuple-y v) mag) (/ (tuple-z v) mag))))

(: dot* (-> Vector Vector Float))
(define (dot* v1 v2)
  (+ (* (tuple-x v1) (tuple-x v2))
     (* (tuple-y v1) (tuple-y v2))
     (* (tuple-z v1) (tuple-z v2))
     (* (tuple-w v1) (tuple-w v2))))

(: cross* (-> Vector Vector Vector))
(define (cross* v1 v2)
  (vec (- (* (tuple-y v1) (tuple-z v2)) (* (tuple-z v1) (tuple-y v2)))
       (- (* (tuple-z v1) (tuple-x v2)) (* (tuple-x v1) (tuple-z v2)))
       (- (* (tuple-x v1) (tuple-y v2)) (* (tuple-y v1) (tuple-x v2)))))

(provide (all-defined-out))
