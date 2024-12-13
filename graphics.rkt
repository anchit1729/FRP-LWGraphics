#lang Racket

(require (except-in racket/gui stream->list stream empty-stream time when stream-tail))
(require sgl
         sgl/gl
         sgl/gl-vectors)
(require "GLWrapper.rkt")
(require "Primitive.rkt")
(require "FRP.rkt")

;(main)  ; Run the program
;(define my-stream (list->stream '(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)))
;(stream->list (gb my-stream))
;(main)

; Window dimensions
(define width 500)
(define height 500)

; Function to draw a circle
(define (draw-circle x y radius)
  (gl-begin 'triangle-fan)
  (gl-vertex x y)  ; Center of the circle
  (for ([i (in-range 33)])  ; Draw 32 segments
    (define angle (* 2 pi (/ i 32)))
    (gl-vertex (+ x (* radius (cos angle)))
               (+ y (* radius (sin angle)))))
  (gl-end))

; Set up the OpenGL environment
(define (init-gl)
  (gl-clear-color 0.0 0.0 0.0 1.0)  ; Black background
  (gl-clear 'color-buffer-bit)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-ortho -1.0 1.0 -1.0 1.0 -1.0 1.0))

; Drawing callback
(define (on-paint gl-context)
  (send gl-context set-current)
  (gl-clear 'color-buffer-bit)
  (gl-color 1.0 0.0 0.0)  ; Red color
  (draw-circle 0.0 0.0 0.5)  ; Draw a circle at (0, 0) with radius 0.5
  (send gl-context swap-buffers))

; Create and display the window
(define (setup-window)
  (define frame (new frame% [label "OpenGL Circle"] [width width] [height height]))
  (define canvas
    (new canvas%
         [parent frame]
         [min-width width]
         [min-height height]
         [paint-callback (lambda (canvas dc)
                           (define gl-context (new gl-context% [parent canvas]))
                           (send gl-context set-current)
                           (init-gl)
                           (on-paint gl-context))]))
  (send frame show #t))

; Run the program
(setup-window)
