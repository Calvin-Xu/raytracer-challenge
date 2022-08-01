#lang typed/racket

(struct tuple ([x : Real] [y : Real] [z : Real] [w : Real]) #:prefab #:type-name Tuple)

(: pt (-> Real Real Real Tuple))
(define (pt x y z)
  (tuple x y z 1))

(: pt? (-> Tuple Boolean))
(define (pt? t)
  (= (tuple-w t) 1))

(: vec (-> Real Real Real Tuple))
(define (vec x y z)
  (tuple x y z 0))

(: vec? (-> Tuple Boolean))
(define (vec? t)
  (= (tuple-w t) 0))

(: f= (-> Real Real Boolean))
(define (f= a b)
  (: EPSILON Real)
  (define EPSILON 0.00001)
  (< (abs (- a b)) EPSILON))

(: tuple+ (-> Tuple Tuple Tuple))
(define (tuple+ t1 t2)
  (tuple (+ (tuple-x t1) (tuple-x t2))
         (+ (tuple-y t1) (tuple-y t2))
         (+ (tuple-z t1) (tuple-z t2))
         (+ (tuple-w t1) (tuple-w t2))))

(: tuples+ (-> Tuple * Tuple))
(define (tuples+ . tuples)
  (foldl tuple+ (tuple 0 0 0 0) tuples))

(: tuple- (-> Tuple Tuple Tuple))
(define (tuple- t1 t2)
  (tuple (- (tuple-x t1) (tuple-x t2))
         (- (tuple-y t1) (tuple-y t2))
         (- (tuple-z t1) (tuple-z t2))
         (- (tuple-w t1) (tuple-w t2))))

(: tuples- (-> Tuple * Tuple))
(define (tuples- . tuples)
  (foldl tuple+ (car tuples) (map -tuple (cdr tuples))))

(: -tuple (-> Tuple Tuple))
(define (-tuple t)
  (tuple (- (tuple-x t)) (- (tuple-y t)) (- (tuple-z t)) (- (tuple-w t))))

(: tuple* (-> Tuple Real Tuple))
(define (tuple* t s)
  (tuple (* (tuple-x t) s) (* (tuple-y t) s) (* (tuple-z t) s) (* (tuple-w t) s)))

(: tuple/ (-> Tuple Real Tuple))
(define (tuple/ t s)
  (tuple (/ (tuple-x t) s) (/ (tuple-y t) s) (/ (tuple-z t) s) (/ (tuple-w t) s)))

(: mag (-> Tuple Real))
(define (mag v)
  (if (vec? v)
      (sqrt (+ (sqr (tuple-x v)) (sqr (tuple-y v)) (sqr (tuple-z v))))
      (error "Is not a vector:" v)))

(: norm (-> Tuple Tuple))
(define (norm v)
  (if (vec? v)
      (let ([mag : Real (mag v)])
        (vec (/ (tuple-x v) mag) (/ (tuple-y v) mag) (/ (tuple-z v) mag)))
      (error "Is not a vector:" v)))

(: dot* (-> Tuple Tuple Real))
(define (dot* v1 v2)
  (if (and (vec? v1) (vec? v2))
      (+ (* (tuple-x v1) (tuple-x v2))
         (* (tuple-y v1) (tuple-y v2))
         (* (tuple-z v1) (tuple-z v2))
         (* (tuple-w v1) (tuple-w v2)))
      (error "Dot product on non-vector:" v1 v2)))

(: cross* (-> Tuple Tuple Tuple))
(define (cross* v1 v2)
  (if (and (vec? v1) (vec? v2))
      (vec (- (* (tuple-y v1) (tuple-z v2)) (* (tuple-z v1) (tuple-y v2)))
              (- (* (tuple-z v1) (tuple-x v2)) (* (tuple-x v1) (tuple-z v2)))
              (- (* (tuple-x v1) (tuple-y v2)) (* (tuple-y v1) (tuple-x v2))))
      (error "Dot product on non-vector:" v1 v2)))

(provide (all-defined-out))
