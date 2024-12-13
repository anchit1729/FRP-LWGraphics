#lang Racket

(require sgl
         sgl/gl
         sgl/gl-vectors
         racket/gui
         "Primitive.rkt")

(provide
 ; Shape drawing functions
 draw-line
 draw-rectangle
 draw-ellipse
 draw-triangle
 draw-circle

 ; Color setting functions
 set-color
 set-background-color
 setup-window

 color
 WHITE
 BLUE
 BLACK
 RED
 GREEN

 ; Transformation functions
 translate
 rotate
 scale)

; Basic color structure
(struct color (r g b))

; Function to set color
(define (set-color c)
  (gl-color (color-r c) (color-g c) (color-b c)) 1)

; Function to set background color
(define (set-background-color c)
  (gl-clear-color (color-r c) (color-g c) (color-b c) 1)
  (gl-clear 'color-buffer-bit))

; Draw a line between points
(define (draw-line v1 v2)
  (gl-begin 'lines)
  (gl-vertex (Vertex-x v1) (Vertex-y v1))
  (gl-vertex (Vertex-x v2) (Vertex-y v2))
  (gl-end))

; Draw a rectangle
(define (draw-rectangle x y width height)
  (gl-begin 'quads)
  (gl-vertex x y)
  (gl-vertex (+ x width) y)
  (gl-vertex (+ x width) (+ y height))
  (gl-vertex x (+ y height))
  (gl-end))

; Draw a circle using triangles
(define (draw-circle x y radius #:segments [segments 32])
  (define (circle-point angle)
    (values
     (+ x (* radius (cos angle)))
     (+ y (* radius (sin angle)))))

  (gl-begin 'triangle-fan)
  (gl-vertex x y)

  (for ([i (in-range (add1 segments))])
    (define angle (* 2 pi (/ i segments)))
    (define-values (px py) (circle-point angle))
    (gl-vertex px py))

  (gl-end))

(define (draw-ellipse x y major-axis minor-axis #:segments [segments 32] #:rotation [rotation 0])
  (define (ellipse-point angle)
    (let* [
           (raw-x (* (/ major-axis 2) (cos angle)))
           (raw-y (* (/ minor-axis 2) (sin angle)))

           (rotated-x (- (* raw-x (cos rotation))
                         (* raw-y (sin rotation))))
           (rotated-y (- (* raw-x (sin rotation))
                         (* raw-y (cos rotation))))

           (final-x (+ x rotated-x))
           (final-y (+ y rotated-y))]

      (values final-x final-y)))

  (gl-begin 'triangle-fan)
  (gl-vertex x y)

  (for ([i (in-range (add1 segments))])
    (define angle (* 2 pi (/ i segments)))
    (define-values (px py) (ellipse-point angle))
    (gl-vertex px py))

  (gl-end))
                        

; Draw a triangle
(define (draw-triangle v1 v2 v3)
  (gl-begin 'triangles)
  (gl-vertex (Vertex-x v1) (Vertex-y v1))
  (gl-vertex (Vertex-x v2) (Vertex-y v2))
  (gl-vertex (Vertex-x v3) (Vertex-y v3))
  (gl-end))

; Transformation functions
(define (translate x y)
  (gl-translate x y 0))

(define (rotate angle)
  (gl-rotate angle 0 0 1))

(define (scale x-scale y-scale)
  (gl-scale x-scale y-scale 1))

; Example predefined colors
(define WHITE (color 1.0 1.0 1.0))
(define BLACK (color 0.0 0.0 0.0))
(define RED (color 1.0 0.0 0.0))
(define GREEN (color 0.0 1.0 0.0))
(define BLUE (color 0.0 0.0 1.0))

; Initialize the window and OpenGL context
(define (setup-window width height title)
  (define canvas
    (new canvas%
         [min-width width]
         [min-height height]
         [style '(gl)]
         [paint-callback (lambda (canvas dc) (void))]))
  (send canvas set-title title)
  canvas)