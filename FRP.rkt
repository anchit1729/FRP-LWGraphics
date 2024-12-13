#lang Racket

(provide
 just
 nothing
 stream
 stream-head
 stream-tail
 list->stream
 stream->list
 empty-stream
 empty-stream?
 time
 lift0
 lift1
 lift2
 lift3
 integral
 event-map
 event-choice
 snapshot
 when
 lifted-sin
 greater-than-five
 gb
 mapped-event
 const-behaviour)

(define (const value)
  (lambda (any)
    value))

; Maybe type (for events)
(struct just (value) #:transparent)
(struct nothing () #:transparent)

; Simple stream structure
(struct stream (head tail))

; Function to create stream from list
(define (list->stream lst)
  (if (null? lst)
      empty-stream
      (stream (car lst) (delay (list->stream (cdr lst))))))

; Function to convert back to a list
(define (stream->list strm)
  (if (empty-stream? strm)
      '()
      (cons (stream-head strm) (stream->list (force (stream-tail strm))))))

; Empty stream
(define empty-stream (stream '() (delay empty-stream)))

; Empty stream predicate
(define (empty-stream? strm) (eq? strm empty-stream))

; The simplest Behaviour - Time
(define time (lambda (ts) ts))

; Lifting-related operators
; lift0
(define (lift0 value)
  (lambda (timestream)
    (if (empty-stream? timestream)
        empty-stream
        (stream value (delay ((lift0 value) (force (stream-tail timestream))))))))

; lift1
(define (lift1 f)
  (lambda (strm)
    (if (empty-stream? strm)
        empty-stream
        (stream (f (stream-head strm))
                (delay ((lift1 f) (force (stream-tail strm))))))))

; lift2
(define (lift2 f)
  (lambda (strm1 strm2)
    (if (or (empty-stream? strm1) (empty-stream? strm2))
        empty-stream
        (stream (f (stream-head strm1) (stream-head strm2))
                (delay ((lift2 f) (force (stream-tail strm1)) (force (stream-tail strm2))))))))

; lift3
(define (lift3 f)
  (lambda (strm1 strm2 strm3)
    (if (or (empty-stream? strm1) (empty-stream? strm2) (empty-stream? strm3))
        empty-stream
        (stream (f (stream-head strm1) (stream-head strm2) (stream-head strm3))
                (delay ((lift3 f) (force (stream-tail strm1)) (force (stream-tail strm2)) (force (stream-tail strm3))))))))

; Integrals
(define (integral fb)
  (define (loop prev-t prev-acc ts as)
    (if (or (empty-stream? ts) (empty-stream? as))
        empty-stream
        (let* ((t (stream-head ts))
               (a (stream-head as))
               (acc (+ prev-acc (* (- t prev-t) a))))
          (stream acc
                  (delay (loop t acc (force (stream-tail ts)) (force (stream-tail as))))))))

  (lambda (ts)
    (if (empty-stream? ts)
        empty-stream
        (stream 0 (delay (loop (stream-head ts) 0 (force (stream-tail ts)) (fb ts)))))))

; Event mapping operator
(define (event-map fe f)
  (lambda (timestream)
    (if (empty-stream? timestream)
        empty-stream
        (let ((event (stream-head (fe timestream))))
          (stream (match event
                    [(just v)
                     (just (f v))]  ; Apply the function to the value inside 'just'
                    [(nothing) (nothing)])   ; Leave 'nothing' unchanged
                  (delay ((event-map fe f) (force (stream-tail timestream)))))))))

; Choice operator
(define (event-choice fe1 fe2)
  (lambda (timestream)
    (if (empty-stream? timestream)
        empty-stream
        (let* ((event1 (stream-head (fe1 timestream)))
               (event2 (stream-head (fe2 timestream))))
          (stream (match (list event1 event2)
                         [(list (nothing) (nothing)) (nothing)]
                         [(list (just x) _) (just x)]
                         [(list _ (just x)) (just x)])
                  (delay ((event-choice fe1 fe2) (force (stream-tail timestream)))))))))

; Snapshot operator: Combine event and behavior streams
(define (snapshot fe fb)
  (lambda (timestream)
    (if (empty-stream? timestream)
        empty-stream
        (let* ((event (stream-head (fe timestream)))
               (behavior (stream-head (fb timestream))))
          (stream (match event
                         [(just x) (just (cons x behavior))]
                         [(nothing) (nothing)])
                  (delay ((snapshot fe fb) (force (stream-tail timestream)))))))))

; Until operator
(define (untilFRP fb fe)
  (lambda (timestream)
    (define (loop ts ev-stream old-behaviour)
      (cond
        [(empty-stream? ts) empty-stream]
        [(empty-stream? ev-stream) 
         (stream (stream-head (old-behaviour ts))
                 (delay (loop (force (stream-tail ts)) (force (stream-tail ev-stream)) old-behaviour)))]
        [else
         (let ([event (stream-head ev-stream)])
           (match event
             [(nothing)
              (stream (stream-head (old-behaviour ts))
                      (delay (loop (force (stream-tail ts)) (force (stream-tail ev-stream)) old-behaviour)))]
             [(just new-behaviour)
              (stream (stream-head (new-behaviour ts))
                      (delay (loop (force (stream-tail ts)) (force (stream-tail ev-stream)) old-behaviour)))]))]))
    (loop timestream (fe timestream) fb)))


; when operator: Trigger an event on first state transition False -> True
(define (when fb)
  (lambda (ts)
    (define (loop prev-bool bs)
      (cond
        [(empty-stream? bs) empty-stream] ; If boolean stream is empty, stop
        [else
         (let ([b (stream-head bs)])
           (cond
             ; Transition from False to True -> produce event
             [(and (not prev-bool) b) ; False -> True transition
              (stream (just 'True) (delay (loop b (force (stream-tail bs)))))]
             ; Otherwise no event
             [else
              (stream (nothing) (delay (loop b (force (stream-tail bs)))))]))]))
    (loop #f (fb ts)))) ; Start with prev-bool as False

; Lift the sin operator
(define lifted-sin (lift1 sin))

; Tests
(define (example-behaviour timestream)
  (if (empty-stream? timestream)
      empty-stream
      (stream (* 2 (stream-head timestream))
              (delay (example-behaviour (force (stream-tail timestream)))))))
(define sin-behaviour
  (lambda (timestream)
  (if (empty-stream? timestream)
      empty-stream
      (stream (sin (stream-head timestream)) (delay (sin-behaviour (force (stream-tail timestream))))))))
(define square-behaviour
  (lambda (timestream)
  (if (empty-stream? timestream)
      empty-stream
      (stream (sqr (stream-head timestream)) (delay (square-behaviour (force (stream-tail timestream))))))))


(define (custom-event timestream)
  (if (empty-stream? timestream)
      empty-stream
      (let ([t (stream-head timestream)])
        (if (< t 0.5)
            (stream (just square-behaviour)
                    (delay (custom-event (force (stream-tail timestream)))))
            (stream (nothing)
                    (delay (custom-event (force (stream-tail timestream)))))))))

(define my-stream (list->stream '(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)))
(define stream1 (list->stream '(1 2 3 4 5)))
(define stream2 (list->stream '(10 20 30 40 50)))
(define stream3 (list->stream '(100 200 300 400 500)))
(define (custom1-event timestream)
  (if (empty-stream? timestream)
      empty-stream
      (let ([t (stream-head timestream)])
        (if (> t 0.5)
            (stream (just 1321) (delay (custom-event (force (stream-tail timestream)))))
            (stream (nothing) (delay (custom-event (force (stream-tail timestream)))))))))


(define choice (event-choice custom-event custom1-event))
;(stream->list (choice my-stream))
(define mapped (event-map custom1-event (lambda (x) (* 2 x))))
;(stream->list (mapped my-stream))
(define snaps (snapshot custom1-event square-behaviour))
;(stream->list (snaps my-stream))
;(stream->list (lifted-sin my-stream))
(define ets (custom-event my-stream))
;(stream->list ets)
;(stream->list (time my-stream))
(define switched-behaviour (untilFRP time custom-event))

(define greater-than-five
  (lambda (ts)
    ((lift2 >) ts ((lift0 0.5) ts))))
(define pred (when greater-than-five))
;(stream->list (greater-than-five my-stream))
;(stream->list (pred my-stream))
(define const-behaviour
  (lambda (ts)
    (lift0 1.0) ts))
(define mapped-event (event-map pred (const (lift0 1.5))))
(define gb (untilFRP const-behaviour mapped-event))
(define sum-stream ((lift2 +) stream1 stream2))
(define combined-stream ((lift3 (lambda (x y z) (+ x y z))) stream1 stream2 stream3))
(define lifted-constant (lift0 5))
(define lifted-double (lambda (x) (* x 2)))
(define integral-sine-wave (integral square-behaviour))

;(stream->list (example-behaviour my-stream))
;(stream->list (sin-behaviour my-stream))
;(stream->list (lifted-constant my-stream))
(stream->list ((lift1 (lambda (x) (* x 2))) my-stream))
(stream->list combined-stream)
;(stream->list sum-stream)
;(stream->list (integral-sine-wave my-stream))
(stream->list (switched-behaviour my-stream))