#lang typed/racket

(struct tuple ([x : Real] [y : Real] [z : Real] [w : Real]) #:prefab)

(: pt (-> Real Real Real tuple))
(define (pt x y z)
  (tuple x y z 1))

(: pt? (-> tuple Boolean))
(define (pt? t)
  (= (tuple-w t) 1))

(: vec (-> Real Real Real tuple))
(define (vec x y z)
  (tuple x y z 0))

(: vec? (-> tuple Boolean))
(define (vec? t)
  (= (tuple-w t) 0))

(: f= (-> Real Real Boolean))
(define (f= a b)
  (: EPSILON Real)
  (define EPSILON 0.00001)
  (< (abs (- a b)) EPSILON))

(: tuple+ (-> tuple tuple tuple))
(define (tuple+ t1 t2)
  (tuple (+ (tuple-x t1) (tuple-x t2))
         (+ (tuple-y t1) (tuple-y t2))
         (+ (tuple-z t1) (tuple-z t2))
         (+ (tuple-w t1) (tuple-w t2))))

(: tuples+ (-> tuple * tuple))
(define (tuples+ . tuples)
  (foldl tuple+ (tuple 0 0 0 0) tuples))

(: tuple- (-> tuple tuple tuple))
(define (tuple- t1 t2)
  (tuple (- (tuple-x t1) (tuple-x t2))
         (- (tuple-y t1) (tuple-y t2))
         (- (tuple-z t1) (tuple-z t2))
         (- (tuple-w t1) (tuple-w t2))))

(: tuples- (-> tuple * tuple))
(define (tuples- . tuples)
  (foldl tuple+ (car tuples) (map -tuple (cdr tuples))))

(: -tuple (-> tuple tuple))
(define (-tuple t)
  (tuple (- (tuple-x t)) (- (tuple-y t)) (- (tuple-z t)) (- (tuple-w t))))

(: tuple* (-> tuple Real tuple))
(define (tuple* t s)
  (tuple (* (tuple-x t) s) (* (tuple-y t) s) (* (tuple-z t) s) (* (tuple-w t) s)))

(: tuple/ (-> tuple Real tuple))
(define (tuple/ t s)
  (tuple (/ (tuple-x t) s) (/ (tuple-y t) s) (/ (tuple-z t) s) (/ (tuple-w t) s)))

(: mag (-> tuple Real))
(define (mag v)
  (if (vec? v)
      (sqrt (+ (sqr (tuple-x v)) (sqr (tuple-y v)) (sqr (tuple-z v))))
      (error "Is not a vector:" v)))

(: norm (-> tuple tuple))
(define (norm v)
  (if (vec? v)
      (let ([mag : Real (mag v)])
        (vec (/ (tuple-x v) mag) (/ (tuple-y v) mag) (/ (tuple-z v) mag)))
      (error "Is not a vector:" v)))

(: dot* (-> tuple tuple Real))
(define (dot* v1 v2)
  (if (and (vec? v1) (vec? v2))
      (+ (* (tuple-x v1) (tuple-x v2))
         (* (tuple-y v1) (tuple-y v2))
         (* (tuple-z v1) (tuple-z v2))
         (* (tuple-w v1) (tuple-w v2)))
      (error "Dot product on non-vector:" v1 v2)))
