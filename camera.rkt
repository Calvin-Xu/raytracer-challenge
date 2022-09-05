#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")
(require "color.rkt")
(require "canvas.rkt")
(require "ray.rkt")
(require "shading.rkt")
(require "world.rkt")
(require typed/racket/flonum)

(define randgen (current-pseudo-random-generator))

(struct camera
        ([hsize : Exact-Positive-Integer] [vsize : Exact-Positive-Integer]
                                             [fov : Float]
                                             [transform : Matrix]
                                             [focal-length : Float]
                                             [aparture-size : Float]
                                             [inv-trans : Matrix])
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
  (camera hsize vsize fov transform focal-length aparture-size (inverse transform)))

(: camera-half-width-height (-> Camera (Pair Float Float)))
(define (camera-half-width-height c)
  (let* ([half-view : Float (fl* (camera-focal-length c) (tan (fl/ (camera-fov c) 2.)))]
         [aspect : Float (fl/ (exact->inexact (camera-hsize c)) (exact->inexact (camera-vsize c)))]
         [half-width : Float (if (fl>= aspect 1.) half-view (fl* half-view aspect))]
         [half-height : Float (if (fl>= aspect 1.) (fl/ half-view aspect) half-view)])
    (cons half-width half-height)))

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
         [pixel (mat-t* (camera-inv-trans c)
                        (pt world-x world-y (- (camera-focal-length c))))])
    (: make-ray (-> Point Ray))
    (define (make-ray origin)
      (let* ([transformed-origin (assert (mat-t* (camera-inv-trans c) origin) point?)]
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

(require racket/future)

(: render
   (->* (World Camera)
        (Exact-Positive-Integer Exact-Positive-Integer Exact-Positive-Integer)
        Canvas))
(define (render w c [n-rays 1] [n-threads 1] [n-reflect-refract 5])
  (: render-pixel (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer Color))
  (define (render-pixel x y)
    (let* ([rays : (Listof Ray)
            (rays-to-pixel c x y n-rays)]
           [colors : (Listof Color)
            (map (lambda ([ray : Ray]) (shade-ray w ray n-reflect-refract)) rays)]
           [average : Color
            (if (= n-rays 1)
                (car colors)
                (color/ (apply colors+ colors) (exact->inexact (length rays))))])
      average))
  (let* ([width (camera-hsize c)]
         [height (camera-vsize c)]
         [n-total (* width height)]
         [n-each (quotient n-total n-threads)])
    (: render-slice (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer (Vectorof Color)))
    (define (render-slice n-todo n-before)
      (build-vector n-todo
                    (lambda ([n : Exact-Nonnegative-Integer])
                      (render-pixel (remainder (+ n n-before) width)
                                    (quotient (+ n n-before) width)))))
    (let ([slices : (Listof (Futureof (Vectorof Color)))
           (for/fold ([acc : (Listof (Futureof (Vectorof Color))) '()]
                      #:result (reverse acc))
                     ([i (in-range 0 n-threads)])
             (cons ((inst future (Vectorof Color))
                    (thunk (render-slice n-each (assert (* n-each i) nonnegative-integer?))))
                   acc))])
      (canvas width
              height
              (vector->immutable-vector
               (vector-append (apply vector-append (map (inst touch (Vectorof Color)) slices))
                              (render-slice (+ n-each (remainder n-total n-threads))
                                            (* (sub1 n-threads) n-each))))))))
