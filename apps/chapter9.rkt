#lang typed/racket
(require "../tuples.rkt")
(require "../color.rkt")
(require "../canvas.rkt")
(require "../transform.rkt")
(require "../shapes.rkt")
(require "../material.rkt")
(require "../light.rkt")
(require "../shading.rkt")
(require "../world.rkt")
(require "../camera.rkt")

(define common-material (make-material #:color (color 1. 0.9 0.9) #:specular 0.))

(define floor (plane "floor" #:material common-material))

(define middle
  (sphere "middle-sphere"
          #:transformation (translate -0.5 1. 0.5)
          #:material (make-material #:color (color 0.1 1. 0.5) #:diffuse 0.7 #:specular 0.3)))

(define right
  (sphere "right-sphere"
          #:transformation (transformation (scale 0.5 0.5 0.5) (translate 1.5 0.5 -0.5))
          #:material (make-material #:color (color 0.5 1. 0.1) #:diffuse 0.7 #:specular 0.3)))

(define left
  (sphere "left-sphere"
          #:transformation (transformation (scale 0.33 0.33 0.33) (translate -1.5 0.33 -0.75))
          #:material (make-material #:color (color 1. 0.8 0.1) #:diffuse 0.7 #:specular 0.3)))

(define world
  (make-world (list floor middle right left)
              (list (point-light "white-above-left" (pt -10. 10. -10.) (color 1. 1. 1.))
                    (point-light "sky-blue-above-right" (pt 5. 10. -10.) (color 0.53 0.81 0.92)))))

(define camera
  (make-camera #:hsize 1080
               #:vsize 720
               #:fov (/ pi 3)
               #:transform (view-transformation (pt 0. 1.5 -5.) (pt 0. 1. 0.) (vec 0. 1. 0.))
               #:aparture-size 0.001))

(save-canvas (render world camera 5) "test.ppm")