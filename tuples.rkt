#lang typed/racket

(struct tuple ([x : Real] [y : Real] [z : Real] [w : Real]) #:prefab #:type-name Tuple)
(struct point tuple () #:prefab #:type-name Point)
(struct vect tuple () #:prefab #:type-name Vector)

(: pt (->* (Real Real Real) (Real) Point))
(define (pt x y z [w 1])
  (point x y z w))

(: pt? (-> Tuple Boolean))
(define (pt? t)
  (= (tuple-w t) 1))

(: vec (->* (Real Real Real) (Real) Vector))
(define (vec x y z [w 0])
  (vect x y z w))

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
  (let* ([xyzw : (List Real Real Real Real)
          (list (+ (tuple-x t1) (tuple-x t2))
                (+ (tuple-y t1) (tuple-y t2))
                (+ (tuple-z t1) (tuple-z t2))
                (+ (tuple-w t1) (tuple-w t2)))]
         [xyz : (List Real Real Real)
          (reverse (cdr (reverse xyzw)))])

    (cond
      [(and (pt? t1) (pt? t2) (error "Illegal operation: point + point" t1 t2))]
      [(or (and (pt? t1) (vec? t2)) (and (pt? t2) (vec? t1))) (apply pt xyz)]
      [(and (vec? t1) (vec? t2)) (apply vec xyz)]
      [else (apply tuple xyzw)])))

(: tuples+ (-> Tuple * Tuple))
(define (tuples+ . tuples)
  (foldl tuple+ (tuple 0 0 0 0) tuples))

(: tuple- (-> Tuple Tuple Tuple))
(define (tuple- t1 t2)
  (let* ([xyzw : (List Real Real Real Real)
          (list (- (tuple-x t1) (tuple-x t2))
                (- (tuple-y t1) (tuple-y t2))
                (- (tuple-z t1) (tuple-z t2))
                (- (tuple-w t1) (tuple-w t2)))]
         [xyz : (List Real Real Real)
          (reverse (cdr (reverse xyzw)))])

    (cond
      [(and (vec? t1) (pt? t2) (error "Illegal operation: vector - point" t1 t2))]
      [(and (pt? t1) (pt? t2)) (apply vec xyz)]
      [(and (vec? t1) (vec? t2)) (apply vec xyz)]
      [(and (pt? t1) (vec? t2)) (apply pt xyz)]
      [else (apply tuple xyzw)])))

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

(: mag (-> Vector Real))
(define (mag v)
  (sqrt (+ (sqr (tuple-x v)) (sqr (tuple-y v)) (sqr (tuple-z v)))))

(: norm (-> Vector Vector))
(define (norm v)
  (let ([mag : Real (mag v)])
    (vec (/ (tuple-x v) mag) (/ (tuple-y v) mag) (/ (tuple-z v) mag))))

(: dot* (-> Vector Vector Real))
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
