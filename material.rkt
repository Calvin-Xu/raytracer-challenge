#lang typed/racket
(provide (all-defined-out))
(require "color.rkt")
(require "matrix.rkt")
(require "patterns.rkt")

(struct material
        ([color : Color] [ambient : Float] [diffuse : Float] [specular : Float] [shininess : Float] [pattern : Pattern])
  #:prefab
  #:type-name Material)

(:
 make-material
 (->* () (#:color Color #:ambient Float #:diffuse Float #:specular Float #:shininess Float #:pattern Pattern) Material))
(define (make-material #:color [color (color 1. 1. 1.)]
                          #:ambient [ambient 0.1]
                          #:diffuse [diffuse 0.9]
                          #:specular [specular 0.9]
                          #:shininess [shininess 200.]
                          #:pattern [pattern (pattern 'plain (list color))])
  (material color ambient diffuse specular shininess pattern))
