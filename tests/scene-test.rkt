#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui
         "../tuples.rkt"
         "../color.rkt"
         "../matrix.rkt"
         "../transform.rkt"
         "../ray.rkt"
         "../intersect.rkt"
         "../lighting.rkt"
         "../material.rkt"
         "../shapes.rkt"
         "../world.rkt"
         "../camera.rkt")

(define-syntax-rule (check-tuple= t1 t2)
    (unless (and (f= (tuple-x t1) (tuple-x t2))
                 (f= (tuple-y t1) (tuple-y t2))
                 (f= (tuple-z t1) (tuple-z t2))
                 (f= (tuple-w t1) (tuple-w t2)))
      (printf "Failure: tuples not equal ~v, ~v\n" t1 t2)))

(define scene-test
  (test-suite
   "Making a Scene"
   (test-suite
    "Building a World"
    (test-case "Creating a world"
               (define w make-world)
               (check-equal? (hash-values (world-objects w)) '())
               (check-equal? (hash-values (world-lights w)) '()))
    (test-case
     "The default world"
     (define w default-world)
     (check-equal? (hash-ref (world-lights w) "default light")
                   (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))
     (check-equal?
      (hash-ref (world-objects w) "outer concentric sphere")
      (sphere "outer concentric sphere"
              #:material (make-material #:color (color 0.8 1.0 0.6) #:diffuse 0.7 #:specular 0.2)))
     (check-equal? (hash-ref (world-objects w) "inner concentric sphere")
                   (sphere "inner concentric sphere" #:transformation (scale 0.5 0.5 0.5))))
    (test-case "Intersect a world with a ray"
               (define w default-world)
               (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
               (define xs (intersect-world w r))
               (check-equal? (length xs) 4)
               (check-equal? (intersection-t (first xs)) 4.)
               (check-equal? (intersection-t (second xs)) 4.5)
               (check-equal? (intersection-t (third xs)) 5.5)
               (check-equal? (intersection-t (fourth xs)) 6.))
    (test-case "Precomputing the state of an intersection"
               (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
               (define s (sphere "s"))
               (define i (intersection 4. s))
               (define comps (precomp i r))
               (check-equal? (intersection-data-t comps) (intersection-t i))
               (check-equal? (intersection-data-object comps) (intersection-obj i))
               (check-tuple= (intersection-data-point comps) (pt 0. 0. -1.))
               (check-tuple= (intersection-data-eyev comps) (vec 0. 0. -1.))
               (check-tuple= (intersection-data-normalv comps) (vec 0. 0. -1.)))
    (test-case "The hit, when an intersection occurs on the outside"
               (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
               (define s (sphere "s"))
               (define i (intersection 4. s))
               (define comps (precomp i r))
               (check-equal? (intersection-data-inside comps) #f))
    (test-case "The hit, when an intersection occurs on the inside"
               (define r (ray (pt 0. 0. 0.) (vec 0. 0. 1.)))
               (define s (sphere "s"))
               (define i (intersection 1. s))
               (define comps (precomp i r))
               (check-tuple= (intersection-data-point comps) (pt 0. 0. 1.))
               (check-tuple= (intersection-data-eyev comps) (vec 0. 0. -1.))
               (check-equal? (intersection-data-inside comps) #t)
               (check-tuple= (intersection-data-normalv comps) (vec 0. 0. -1.)))
    (test-case "Shading an intersection"
               (define w default-world)
               (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
               (define s (hash-ref (world-objects w) "outer concentric sphere"))
               (define i (intersection 4. s))
               (define comps (precomp i r))
               (define c (shade w comps))
               (check-color= c (color 0.38066 0.47583 0.2855)))
    (test-case
     "Shading an intersection from the inside"
     (define w0 (add-light default-world (point-light "l" (pt 0. 0.25 0.) (color 1. 1. 1.))))
     (define w (world (world-objects default-world) (hash-remove (world-lights w0) "default light")))
     (define r (ray (pt 0. 0. 0.) (vec 0. 0. 1.)))
     (define s (hash-ref (world-objects w) "inner concentric sphere"))
     (define i (intersection 0.5 s))
     (define comps (precomp i r))
     (define c (shade w comps))
     (check-color= c (color 0.90498 0.90498 0.90498)))
    (test-case "The color when a ray misses"
               (define r (ray (pt 0. 0. -5.) (vec 0. 1. 0.)))
               (check-color= (shade-ray default-world r) black))
    (test-case "The color when a ray hits"
               (define r (ray (pt 0. 0. -5.) (vec 0. 0. 1.)))
               (check-color= (shade-ray default-world r) (color 0.38066 0.47583 0.2855)))
    (test-case
     "The color with an intersection behind the ray"
     (define w
       (let* ([w1 make-world]
              [w2 (add-light w1 (point-light "default light" (pt -10. 10. -10.) (color 1. 1. 1.)))]
              [w3 (add-object w2
                              (sphere "outer concentric sphere"
                                      #:material (make-material #:color (color 0.8 1.0 0.6)
                                                                #:ambient 1.
                                                                #:diffuse 0.7
                                                                #:specular 0.2)))]
              [w4 (add-object w3
                              (sphere "inner concentric sphere"
                                      #:transformation (scale 0.5 0.5 0.5)
                                      #:material (make-material #:ambient 1.)))])
         w4))
     (define r (ray (pt 0. 0. 0.75) (vec 0. 0. -1.)))
     (check-color= (shade-ray w r)
                   (material-color (shape-material (hash-ref (world-objects w)
                                                             "inner concentric sphere"))))))
   (test-suite "Defining a View Transformation"
               (test-case "The transformation matrix for the default orientation"
                          (define from (pt 0. 0. 0.))
                          (define to (pt 0. 0. -1.))
                          (define up (vec 0. 1. 0.))
                          (check-true (mat= (view-transformation from to up) id-mat-4)))
               (test-case "A view transformation matrix looking in positive z direction"
                          (define from (pt 0. 0. 0.))
                          (define to (pt 0. 0. 1.))
                          (define up (vec 0. 1. 0.))
                          (check-true (mat= (view-transformation from to up) (scale -1. 1. -1.))))
               (test-case "The view transformation moves the world"
                          (define from (pt 0. 0. 8.))
                          (define to (pt 0. 0. 0.))
                          (define up (vec 0. 1. 0.))
                          (check-true (mat= (view-transformation from to up) (translate 0. 0. -8.))))
               (test-case "An arbitrary view transformation"
                          (define from (pt 1. 3. 2.))
                          (define to (pt 4. -2. 8.))
                          (define up (vec 1. 1. 0.))
                          (check-true (mat= (view-transformation from to up)
                                            (mat 4
                                                 4
                                                 #[#[-0.50709 0.50709 0.67612 -2.36643]
                                                   #[0.76772 0.60609 0.12122 -2.82843]
                                                   #[-0.35857 0.59761 -0.71714 0.00000]
                                                   #[0.00000 0.00000 0.00000 1.00000]])))))
   (test-suite
    "Implementing a Camera"
    (test-case "Constructing a camera"
               (define c (make-camera #:hsize 160 #:vsize 120 #:fov (/ pi 2)))
               (check-equal? (camera-hsize c) 160)
               (check-equal? (camera-vsize c) 120)
               (check-equal? (camera-fov c) (/ pi 2))
               (check-equal? (camera-transform c) id-mat-4))
    (test-case "The pixel size for a horizontal canvas"
               (define c (make-camera #:hsize 200 #:vsize 125 #:fov (/ pi 2)))
               (check-= (camera-pixel-size c) 0.01 0.00001))
    (test-case "The pixel size for a vertical canvas"
               (define c (make-camera #:hsize 125 #:vsize 200 #:fov (/ pi 2)))
               (check-= (camera-pixel-size c) 0.01 0.00001))
    (test-case "Constructing a ray through the center of the canvas"
               (define c (make-camera #:hsize 201 #:vsize 101 #:fov (/ pi 2)))
               (define r (car (rays-to-pixel c 100 50)))
               (check-tuple= (ray-origin r) (pt 0. 0. 0.))
               (check-tuple= (ray-direction r) (vec 0. 0. -1.)))
    (test-case "Constructing a ray through a corner of the canvas"
               (define c (make-camera #:hsize 201 #:vsize 101 #:fov (/ pi 2)))
               (define r (car (rays-to-pixel c 0 0)))
               (check-tuple= (ray-origin r) (pt 0. 0. 0.))
               (check-tuple= (ray-direction r) (vec 0.66519 0.33259 -0.66851)))
    (test-case "Constructing a ray when the camera is transformed"
               (define c
                 (make-camera #:hsize 201
                              #:vsize 101
                              #:fov (/ pi 2)
                              #:transform (mat* (rotate 'y (/ pi 4)) (translate 0. -2. 5.))))
               (define r (car (rays-to-pixel c 100 50)))
               (check-tuple= (ray-origin r) (pt 0. 2. -5.))
               (check-tuple= (ray-direction r) (vec (/ (sqrt 2.) 2.) 0. (- (/ (sqrt 2.) 2.)))))
    (test-case "Rendering a world with a camera"))))

(run-tests scene-test)
