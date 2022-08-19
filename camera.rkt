#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "color.rkt")
(require "canvas.rkt")
(require "ray.rkt")
(require "shading.rkt")
(require "intersect.rkt")
(require "world.rkt")

(define randgen (current-pseudo-random-generator))

(struct camera
        ([hsize : Exact-Positive-Integer] [vsize : Exact-Positive-Integer]
                                             [fov : Float]
                                             [transform : Matrix]
                                             [focal-length : Float]
                                             [aparture-size : Float])
  #:prefab
  #:type-name Camera)

(: make-camera
   (->* (#:hsize Exact-Positive-Integer #:vsize Exact-Positive-Integer #:fov Float)
        (#:transform Matrix #:focal-length Float #:aparture-size Float)
        Camera))
(define (make-camera #:hsize hsize
                     #:vsize vsize
                     #:fov fov
                     #:transform [transform id-mat-4]
                     #:focal-length [focal-length 1.]
                     #:aparture-size [aparture-size 0.002])
  (camera hsize vsize fov transform focal-length aparture-size))

(: camera-half-width-height (-> Camera (Pair Float Float)))
(define (camera-half-width-height c)
  (let* ([half-view : Float (* (camera-focal-length c) (tan (/ (camera-fov c) 2.)))]
         [aspect : Float (/ (exact->inexact (camera-hsize c)) (exact->inexact (camera-vsize c)))]
         [half-width : Float (if (>= aspect 1.) half-view (* half-view aspect))]
         [half-height : Float (if (>= aspect 1.) (/ half-view aspect) half-view)])
    (cons (cast half-width Float) (cast half-height Float))))

(: camera-pixel-size (-> Camera Float))
(define (camera-pixel-size c)
  (let* ([half-width-height (camera-half-width-height c)])
    (/ (* 2. (car half-width-height)) (exact->inexact (camera-hsize c)))))

(: rays-to-pixel
   (->* (Camera Exact-Nonnegative-Integer Exact-Nonnegative-Integer)
        (Exact-Positive-Integer)
        (Listof Ray)))
(define (rays-to-pixel c x y [nrays 1])
  (let* ([pixel-size (camera-pixel-size c)]
         [x-offset (* (+ (exact->inexact x) 0.5) pixel-size)]
         [y-offset (* (+ (exact->inexact y) 0.5) pixel-size)]
         [half-width-height (camera-half-width-height c)]
         [world-x (- (car half-width-height) x-offset)]
         [world-y (- (cdr half-width-height) y-offset)]
         [pixel (mat-t* (inverse (camera-transform c))
                        (pt world-x world-y (- (camera-focal-length c))))])
    (: make-ray (-> Point Ray))
    (define (make-ray origin)
      (let* ([transformed-origin (assert (mat-t* (inverse (camera-transform c)) origin) point?)]
             [direction (norm (assert (tuple- pixel transformed-origin) vect?))])
        (ray transformed-origin direction)))
    (for/fold ([rays : (Listof Ray) '()])
              ([i (in-range 0 nrays)])
      (if (= i 0)
          ((inst cons Ray) (make-ray (pt 0. 0. 0.)) rays)
          (let* ([random (random randgen)]
                 [sign (if (> random 0.5) + -)]
                 [offset (sign (* random (camera-aparture-size c)))])
            ((inst cons Ray) (make-ray (pt offset offset 0.)) rays))))))

(: render (->* (World Camera) (Exact-Positive-Integer) Canvas))
(define (render w c [nrays 1])
  (build-canvas (camera-hsize c)
                (camera-vsize c)
                (lambda (x y)
                  (let* ([rays : (Listof Ray) (rays-to-pixel c x y nrays)]
                         [colors : (Listof Color)
                          (map (lambda ([ray : Ray]) (shade-ray w ray)) rays)]
                         [average : Color
                          (if (= nrays 1)
                              (car colors)
                              (color/ (apply colors+ colors) (exact->inexact (length rays))))])
                    average))))
