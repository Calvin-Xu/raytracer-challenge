#lang typed/racket
(require "tuples.rkt")

(struct _color tuple () #:prefab #:type-name Color)

(: color (->* (Real Real Real) (Real) Color))
(define (color red green blue [alpha 1])
  (_color red green blue alpha))

(define color? _color?)

(: color-r (-> Color Real))
(define (color-r color)
  (tuple-x color))

(: color-g (-> Color Real))
(define (color-g color)
  (tuple-y color))

(: color-b (-> Color Real))
(define (color-b color)
  (tuple-z color))

(: color-a (-> Color Real))
(define (color-a color)
  (tuple-w color))

(: color->string (->* (Color) (Exact-Nonnegative-Integer) String))
(define (color->string color [max_color_val 255])
  (: scale (-> Real Integer))
  (define (scale frac)
    (exact-round (* (max 0 (min 1.0 frac)) max_color_val)))
  (string-append (number->string (scale (color-r color)))
                 " "
                 (number->string (scale (color-g color)))
                 " "
                 (number->string (scale (color-b color)))
                 " "))

(: color-op (-> (-> Real Real * Real) Color Color Color))
(define (color-op op c1 c2)
  (color (op (color-r c1) (color-r c2))
         (op (color-g c1) (color-g c2))
         (op (color-b c1) (color-b c2))))

(: color+ (-> Color Color Color))
(define (color+ c1 c2)
  (color-op + c1 c2))

(: color- (-> Color Color Color))
(define (color- c1 c2)
  (color-op - c1 c2))

(: color* (-> Color (U Color Real) Color))
(define (color* c arg)
  (color-op * c (if (color? arg) arg (color arg arg arg))))

(struct _canvas
  ([width : Exact-Positive-Integer]
   [height : Exact-Positive-Integer]
   [pixels : (Mutable-Vectorof Color)]) #:prefab #:type-name Canvas)

(: canvas-width (-> Canvas Exact-Positive-Integer))
(define (canvas-width canvas)
  (_canvas-width canvas))

(: canvas-height (-> Canvas Exact-Positive-Integer))
(define (canvas-height canvas)
  (_canvas-height canvas))

(: canvas-pixels (-> Canvas (Mutable-Vectorof Color)))
(define (canvas-pixels canvas)
  (_canvas-pixels canvas))

(: canvas (-> Exact-Positive-Integer Exact-Positive-Integer Canvas))
(define (canvas width height)
  (_canvas width height (make-vector (* width height) (color 0 0 0))))

(: pixel-at (-> Canvas Exact-Nonnegative-Integer Exact-Nonnegative-Integer Color))
(define (pixel-at canvas x y)
  (if (or (>= x (canvas-width canvas)) (>= y (canvas-height canvas)))
      (error "Illegal operation: attempting to access pixel out of bounds" x y)
      (vector-ref (canvas-pixels canvas) (+ (* y (canvas-width canvas)) x))))

(: set-pixel! (-> Canvas Exact-Nonnegative-Integer Exact-Nonnegative-Integer Color Void))
(define (set-pixel! canvas x y color)
  (if (or (>= x (canvas-width canvas)) (>= y (canvas-height canvas)))
      (error "Illegal operation: attempting to set pixel out of bounds" x y)
      (vector-set! (canvas-pixels canvas) (+ (* y (canvas-width canvas)) x) color)))

(: serialize-canvas (-> Canvas String))
(define (serialize-canvas canvas)
  (define header
    (string-append "P3\n"
                   (number->string (canvas-width canvas))
                   " "
                   (number->string (canvas-height canvas))
                   "\n"
                   "255\n"))
  (define bitmap (vector-map color->string (canvas-pixels canvas)))
  ;; color->string always adds whitespace at end
  ;; replace appropriate whitespaces with newlines
  (begin
    (let ([PIXELS_PER_ROW 6] [n (* (canvas-width canvas) (canvas-height canvas))])
      (for ([i (in-range n)])
        (when (or (= (add1 i) n) (= 0 (remainder (add1 i) PIXELS_PER_ROW)))
          (let ([curr (vector-ref bitmap i)])
            (vector-set! bitmap
                         i
                         (string-append (substring curr 0 (sub1 (string-length curr))) "\n"))))))
    (string-append header (string-append* (vector->list bitmap)))))

(: save-canvas (-> Canvas String Void))
(define (save-canvas canvas filename)
  (let ([out (open-output-file filename #:mode 'text #:exists 'replace)])
    (display (serialize-canvas canvas) out)
    (close-output-port out)))

(provide (except-out (all-defined-out) color-op))
