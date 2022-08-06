#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../canvas.rkt")

(define-syntax-rule (check-tuple= t1 t2)
  (unless (and (f= (tuple-x t1) (tuple-x t2))
               (f= (tuple-y t1) (tuple-y t2))
               (f= (tuple-z t1) (tuple-z t2))
               (f= (tuple-w t1) (tuple-w t2)))
    (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define canvas-test
  (test-suite
   "Drawing on a Canvas"
   (test-case "define color"
              (define c (color -0.5 0.4 1.7))
              (check-equal? (color-r c) -0.5)
              (check-equal? (color-g c) 0.4)
              (check-equal? (color-b c) 1.7))
   (test-case "add color"
              (define c1 (color 0.9 0.6 0.75))
              (define c2 (color 0.7 0.1 0.25))
              (check-color= (color+ c1 c2) (color 1.6 0.7 1.0)))
   (test-case "subtract color"
              (define c1 (color 0.9 0.6 0.75))
              (define c2 (color 0.7 0.1 0.25))
              (check-color= (color- c1 c2) (color 0.2 0.5 0.5)))
   (test-case "multiply color"
              (define c (color 0.2 0.3 0.4))
              (check-color= (color* c 2.) (color 0.4 0.6 0.8))
              (define c1 (color 1. 0.2 0.4))
              (define c2 (color 0.9 1. 0.1))
              (check-color= (color* c1 c2) (color 0.9 0.2 0.04)))
   (test-case "color to string"
              (define c (color 0.2 0.3 0.4))
              (check-equal? (color->string c) "51 76 102 "))
   (test-case "create and access canvas"
              (define c (canvas 10 20))
              (check-equal? (canvas-width c) 10)
              (check-equal? (canvas-height c) 20)
              (define red (color 1. 0. 0.))
              (set-pixel! c 2 3 red)
              (check-equal? (pixel-at c 2 3) red))
   (test-case
    "save canvas to ppm file"
    (define c (canvas 5 3))
    (set-pixel! c 0 0 (color 1.5 0. 0.))
    (set-pixel! c 2 1 (color 0. 0.5 0.))
    (set-pixel! c 4 2 (color -0.5 0. 1.))
    (check-equal?
     (serialize-canvas c)
     "P3\n5 3\n255\n255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 128 0 0 0 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 255\n"))))

(run-tests canvas-test)
