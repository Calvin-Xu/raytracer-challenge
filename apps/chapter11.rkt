#lang typed/racket
(require "../tuples.rkt")
(require "../color.rkt")
(require "../canvas.rkt")
(require "../transform.rkt")
(require "../shapes.rkt")
(require "../material.rkt")
(require "../patterns.rkt")
(require "../light.rkt")
(require "../shading.rkt")
(require "../world.rkt")
(require "../camera.rkt")

(define floor
  (plane "floor"
         #:material (make-material #:specular 0.1
                                   #:reflective 0.5
                                   #:pattern (pattern 'checker
                                               (list white black)
                                               #:transformation (scale 0.5 0.5 0.5)))))

(define left-wall
  (plane "left-wall"
         #:transformation (transformation (rotate 'z (/ pi 2)) (translate -1. 0. 0.))
         #:material (make-material #:color (color 1. 0. 0.) #:specular 0.1 #:reflective 0.1)))

(define right-wall
  (plane "right-wall"
         #:transformation (transformation (rotate 'z (/ pi 2)) (translate 1. 0. 0.))
         #:material (make-material #:color (color 0. 1. 0.) #:specular 0.1 #:reflective 0.1)))

(define top-wall
  (plane "top-wall"
         #:transformation (transformation (translate 0. 2. 0.))
         #:material
         (make-material #:color white #:ambient 0.5 #:diffuse 1.0 #:specular 0.1 #:reflective 0.05)))

(define back-wall
  (plane "back-wall"
         #:transformation (transformation (rotate 'x (/ pi 2)) (translate 0. 0. 2.))
         #:material (make-material #:color white #:specular 0.1 #:reflective 0.1)))

(define front-wall
  (plane "front-wall"
         #:transformation (transformation (rotate 'x (/ pi 2)))
         #:material (make-material #:color (color 0. 0. 1.) #:specular 0.1 #:reflective 0.1)))

(define steel-ball
  (sphere "steel-ball"
          #:transformation (transformation (scale 0.5 0.5 0.5) (translate 0. 0.5 1.2))
          #:material (make-material #:color (color-255 192 192 196)
                                    #:transparency 0.
                                    #:reflective 0.8
                                    #:diffuse 0.05
                                    #:ambient 0.05
                                    #:specular 0.9
                                    #:shininess 250.)))

(define glass-ball
  (sphere "glass-ball"
          #:transformation (transformation (scale 0.25 0.25 0.25) (translate 0.4 0.25 0.25))
          #:material (make-material #:color white
                                    #:transparency 0.9
                                    #:reflective 0.9
                                    #:diffuse 0.05
                                    #:ambient 0.05
                                    #:specular 0.9
                                    #:shininess 300.)))

(define stripe-ball
  (sphere "stripe-ball"
          #:transformation (transformation (scale 0.15 0.15 0.15) (translate -0.3 0.15 0.2))
          #:material (make-material #:transparency 0.9
                                    #:reflective 0.9
                                    #:diffuse 0.05
                                    #:ambient 0.05
                                    #:specular 0.9
                                    #:shininess 300.
                                    #:pattern (pattern 'stripe
                                                (list white (color-255 130 160 250))
                                                #:transformation (scale 0.002 0.002 0.002)))))

(define world
  (make-world (list steel-ball glass-ball stripe-ball floor left-wall right-wall top-wall back-wall)
              (list (point-light "white-above" (pt 0. 1.8 0.5) white))))

(define camera
  (make-camera #:hsize 1080
               #:vsize 1080
               #:fov (/ pi 10)
               #:transform (view-transformation (pt 0. 1. -7.) (pt 0. 1. 0.) (vec 0. 1. 0.))
               #:aparture-size 0.0005))

(define camera2
  (make-camera #:hsize 50
               #:vsize 50
               #:fov (/ pi 10)
               #:transform (view-transformation (pt 0. 1. -7.) (pt 0. 1. 0.) (vec 0. 1. 0.))
               #:aparture-size 0.002))

(define camera3
  (make-camera #:hsize 50
               #:vsize 50
               #:fov (/ pi 2)
               #:transform (view-transformation (pt 0. 1.9 1.5) (pt 0. 0. 1.5) (vec 0. 0. 1.))
               #:aparture-size 0.002))

(define camera4
  (make-camera #:hsize 50
               #:vsize 50
               #:fov (/ pi 10)
               #:transform (view-transformation (pt 0. 0. -7.) (pt 0. 0. 0.) (vec 0. 1. 0.))
               #:aparture-size 0.002))

;; (save-canvas (render world camera2 1 10 2) "test.ppm")
(save-canvas (render world camera 5 10 6) "render.ppm")
