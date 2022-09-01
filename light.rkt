#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "color.rkt")

(struct light ([id : String] [position : Point] [intensity : Color]) #:prefab #:type-name Light)
(struct point-light light () #:prefab #:type-name PointLight)
