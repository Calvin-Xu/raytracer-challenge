#lang typed/racket
(provide (all-defined-out))
(require "color.rkt")

(struct canvas
        ([width : Exact-Positive-Integer]
         [height : Exact-Positive-Integer]
         [pixels : (Immutable-Vectorof Color)])
  #:prefab
  #:type-name Canvas)

(: build-canvas
   (-> Exact-Positive-Integer
       Exact-Positive-Integer
       (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer Color)
       Canvas))
(define (build-canvas width height f)
  (canvas width
          height
          (vector->immutable-vector
           (build-vector (ann (* width height) Integer)
                         (lambda ([n : Exact-Nonnegative-Integer])
                           (f (remainder n width) (quotient n width)))))))

(: pixel-at (-> Canvas Exact-Nonnegative-Integer Exact-Nonnegative-Integer Color))
(define (pixel-at canvas x y)
  (if (or (>= x (canvas-width canvas)) (>= y (canvas-height canvas)))
      (error "Illegal operation: access pixel out of bounds" x y)
      (vector-ref (canvas-pixels canvas) (+ (* y (canvas-width canvas)) x))))

(: save-canvas (->* (Canvas String) (Exact-Nonnegative-Integer) Void))
(define (save-canvas canvas filename [max-color-val 255])
  (define header
    (string-append "P3\n"
                   (number->string (canvas-width canvas))
                   " "
                   (number->string (canvas-height canvas))
                   "\n"
                   (number->string max-color-val)
                   "\n"))
  (define PIXELS_PER_ROW 6)
  (: add-newline (-> String String))
  (define (add-newline str)
    (string-append (substring str 0 (sub1 (string-length str))) "\n"))
  (let ([out (open-output-file filename #:mode 'text #:exists 'replace)])
    (display header out)
    (for/fold ([counter : Integer 1])
              ([pixel : Color (in-vector (canvas-pixels canvas))])
      (let ([serialized : String (color->string pixel max-color-val)])
        (display (if (= counter PIXELS_PER_ROW) (add-newline serialized) serialized) out))
      (if (= counter PIXELS_PER_ROW) 1 (add1 counter)))
    (close-output-port out)))
