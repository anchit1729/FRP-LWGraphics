#lang Racket

(require destruct)

(provide
 (struct-out Vertex)
 (struct-out Side)
 (struct-out Radius)
 (struct-out Rectangle)
 (struct-out Ellipse)
 (struct-out RtTriangle)
 (struct-out Polygon)
 )

; Define a type alias for a vertex, represented as a pair of numbers
(struct Vertex (x y))
(struct Side (x))
(struct Radius (x))
  
(struct Rectangle (listOf Side))
(struct Ellipse (Radius))
(struct RtTriangle (Side))
(struct Polygon (listof Vertex))

(provide/contract
   [distBetween (-> Vertex? Vertex? number?)])

(define (distBetween p1 p2)
  (match* (p1 p2)
    [((Vertex x1 y1) (Vertex x2 y2))
     (sqrt (+ (sqr (- x2 x1))
              (sqr (- y2 y1))))]))