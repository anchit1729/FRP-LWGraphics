# FRP-LWGraphics

Implementations of the Functional Reactive Programming (FRP) Domain-Specific Language (DSL) in Haskell and Racket.

This implementation follows the stream-based approach towards defining FRP constructs as outlined in the paper "Functional Reactive Programming from First Principles" [(paper link)](https://doi.org/10.1145/349299.349331). It builds this FRP implementation on top of a lightweight graphics library/wrapper that handles interfacing with OpenGL, based on [these lecture notes](http://www.cs.yale.edu/homes/hudak/SOE/powerpoi.htm) by Paul Hudak that provide a more up-to-date description of FRP for reactive graphics and animations. Two implementations are provided and compared - one in Haskell, and one in Racket.

## Introduction 

Functional Reactive Programming (FRP) emerged as a powerful paradigm for programming reactive systems, first introduced in the seminal 1997 paper on Functional Reactive Animation (Fran) by Elliott and Hudak [LINK HERE]. FRP combines functional programming principles with reactive programming to handle time-varying values and event-based systems in a declarative manner. Since then, there have been multiple follow up works that explore different approaches towards implementing FRP, however, [Elliot himself](https://github.com/conal/talk-2015-essence-and-origins-of-frp?tab=readme-ov-file#readme) has stated that these representations do not capture the two basic requirements for an FRP implementation:

- Precise, Simple Denotation
- Continuous Time

Instead, Elliot mentions that most modern implementations look at FRP from the lens of:

- Graphs
- Updates and Propagation
- Streams
- Doing

While the stream-based representation shown here is useful in the sense that it naturally allows operations such as accumulative integrals and is mathematically proven to correctly model continuous behaviours provided certain conditions related to continuity and convergence (rather, more commonly uniform continuity and uniform convergence) are satisfied by the underlying functions of time captured by the Behaviours, it is not essential to the FRP philosophy. Indeed, the original Fran implementation does not consider streams at all; Behaviours simply accept singular time samples and output singular values in return.

## Functional Reactive Programming: The Basics

So, we've introduced the idea of FRP. Now, let's dive into exactly what it entails and how all its parts come together to enable different features. 

Starting off with the basics - Behaviours and Events.

A Behaviour is defined as a time-varying value; in the stream definition, it looks like this:

```
Behaviour :: [Time] -> [a]
```
This definition is clear in Haskell; however, Racket by default is dynamically typed, and thus this concept of a 'stream transformer', or a function that takes in one stream and produces another stream, is implicitly built into the Racket implementation of FRP provided here, instead of being enforced the same way Haskell does. From an implementation standpoint, this makes FRP much more convenient to implementing Haskell. However, the Racket implementation is still valuable in the sense that it allows us to take a look at a representation where we do not get a lot of benefits for free - lazy evaluation, the stream construct and types are some of these, but we'll get into them as needed later. 

An Event is similar, with a slight distinction. An Event takes in a stream of time samples and returns all event occurrences up to that time:

```
Event :: [Time] -> [Maybe a]
```

Here, the `Maybe` type, which can either contain a value or nothing (`(Just a)` or `Nothing`). This is a built-in construct for Haskell, however the Racket implementation also provides `struct`s that help achieve the same effect (mostly for readability of code):

```
(struct just (value) #:transparent)
(struct nothing () #:transparent)
```

With these basic constructs in mind, let's move on to talk about the underlying FRP constructs and how these were implemented in both Haskell and Racket.

## Key Constructs

  1. Streams

  In Haskell, lazy evaluation is enabled by default. As a result, lists declared in Haskell are lazy, and we can use these benefits for free! Thus, streams are represented by lists in the Haskell         implementation. On the other hand, Racket doesn't have streams built into it. Sure, there is the `racket/streams` library, but to implement FRP, we'd like to get our hands dirty as much as possible within the timeline, since this was an important part of the actual goal of this project: to learn functional programming and implement a stream-based FRP system that is faithful to the original specification and satisfies the basic guarantees and affordances of FRP. By abstracting away how streams work, we don't get to see how everything works under the hood. Indeed, when I first implemented the Haskell FRP library, it took me a while to wrap my head around how streams wouldn't keep growing infinitely, and I didn't really see how laziness works along with garbage collection to save the day until a bit of fiddling around.

  In Racket, streams are defined as:

  ```
  ; Stream struct
  (struct stream (head tail))

  ; Illstrative function to create stream from list
  (define (list->stream lst)
    (if (null? lst)
        empty-stream
        (stream (car lst) (delay (list->stream (cdr lst))))))
  ```

  2. Time

  Continuous time is a first-class construct in FRP, and is the essence of enabling reactivity through FRP, although modern implementations tend to almost exclusively focus on discrete events for UI programming applications (mouse clicks, buttons presses etc.). This means that the `Time` Behaviour simply returns the stream values as they are:

  ```
  (define time (lambda (ts) ts))
  ```

  Of course, we get the much more concise definition in Haskell:

  ```
  time :: Behaviour Float
  time ts = ts
  ```

  3. Operator Lifting

  Lifting is a powerful concept in functional programming, essentially wrapping existing functions/operators in a way that they may operate on another type. The `lift0, lift1, lift2, lift3` operators are implemented as shown below:

  First, for Racket:

  ```
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
  ```

  In Haskell:

  ```
  -- 1: The 'overloaded' dollar-sign operator --
  ($*) :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
  ff $* fb = \ts -> zipWith ($) (ff `at` ts) (fb `at` ts)

  -- 2: The lift0 operator --
  lift0 :: a -> Behaviour a
  lift0 x = map (const x) -- mapping to an infinite stream of constant values --

  -- 3: The lift1 operator --
  lift1 :: (a -> b) -> (Behaviour a -> Behaviour b)
  lift1 f fb = lift0 f $* fb

  -- 4: The lift2 operator --
  lift2 :: (a -> b -> c) -> (Behaviour a -> Behaviour b -> Behaviour c)
  lift2 f fb sb = lift1 f fb $* sb

  -- 5: The lift3 operator --
  lift3 :: (a -> b -> c -> d) -> (Behaviour a -> Behaviour b -> Behaviour c -> Behaviour d)
  lift3 f fb sb tb = lift2 f fb sb $* tb
  ```

  4. Integrals

  Since continuous time is one of FRP's main assets, the notion of computing an integral is quite intuitive especially with the use of streams. We only show the Racket versions from now on:

  ```
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
  ```

  5. Event Mapping

  Another useful construct in FRP is the event mapping operator, which basically implements the `map` operator for event streams. That is, given a pre-existing event stream with certain values, event mapping applies a given function to all `(just val)` type values in the event stream.

  ```
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
  ```

  6. Event Choice

  One more useful concept in FRP is that of choosing between events. This creates a larger, composite event, and allows, for example, behaviours to change their values if one of two events occur (e.g., turn on the screen of a phone either when it is picked up (E1) or if the power button is pressed once (E2)).

  ```
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
  ```

  7. Snapshots

  The notion of a snapshot is quite simple - capture the value of a behaviour along with the event value when a certain event occurs:

  ```
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
  ```

  8. Behaviour Switching

  This is really where we get into the interactions between Behaviour and Event objects. Based on the occurrence of an event, the `untilFRP` operator allows us to change the way a Behaviour 'behaves' if an event occurrence is recorded:

  ```
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
  ```

  9. Predicate Events

  This is the last main construct for FRP as defined in the stream implementation in the original paper. The idea is straightforward, for a Behaviour with the first state transition from False to True at timestep `k`, a predicate event fires at that timestep (i.e., when the condition is first true). This is implemented as:

  ```
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
  ```

The various commented examples included in the `graphics.rkt` and `Main.hs` files may be used to verify the functionality of the two implementations. 

## Known Limitations

Note that there may likely be further optimizations and programming patterns, especially for the Racket implementation, that aren't as 'elegant' as they could be. At the moment, these are due to my lack of in-depth experience with Lisp-like languages (and functional programming overall). However, the aim is to extend these implementations to work with tangible electronic devices for haptic feedback, and thus subsequent iterations will build in further optimizations.

The first limitation that immediately came to mind while working on this project was that in some sense, FRP really seems like a DSL built for the way Haskell represents information and instructions. Even reading through the implementations - although the core FRP libraries do not take up much space, the Racket implementation is much harder to read, at least for eyes that have not yet been exposed to Lisp/Scheme-like syntaxes for long durations of time. In Haskell, everything is concise and reads very similarly to actual mathematical notation - of course, this partly due to the way whitespaces are used in the language, but a large part of this is the amount of abstraction Haskell offers out of the box.

A stylistic tendency that I noticed when working on the Haskell implementation, specifically on the graphics interfacing functions - despite Haskell's 'purity', the majority of graphics programming is done in an imperative style. Thus, the functions dealing with those parts of the code look much less 'declarative' and are subsequently harder to read for fresh eyes - `do` is used all over the place, as are monads to deal with side effects inherent to IO, Windowing and GUI applications. However, due to various factors, some out of my control (health-related), I was unable to implement a graphics-interfacing layer for the Racket FRP implementation. I did implement graphics interfacing modules for the Haskell implementation, following Conal Elliot's lecture notes, but was not able to port this implementation over to my Racket FRP library in time, and sincerely apologize for not being able to provide this deliverable in my submission. I have included some source files that I was working with to prototype Fran on Racket, but they do not execute properly.

## Animations - Functional Reactive Animation

Coming to the main attraction, slightly dulled by my failure to provide fully-functioning implementations in both Haskell and Racket, this project implements a version of Functional Reactive Animation using the Haskell FRP implementation described earlier. The following abstractions are defined in code:

1. `Primitive` - Mathematical representations of the basic shapes the library exposes.
2. `Graphic` - Wrapper for OpenGL IO interfacing, i.e. the code that does most of the heavy lifting for rendering.
3. `Region` - Container for primitives, this abstraction allows primitives to be stacked and composed in different ways.
4. `Drawing` - A similar wrapper to compose regions, and introduce colours.
5. `Animation` - A stream transformer (i.e., pretty much a Behaviour) that outputs different `Drawing` instances for each time-step.

Examples are included in code, and some fancier animations are shown in the video.

## Alternative Approaches

## Future Work/Applications

## Note

My initial project pitch involved writing a mini-interpreter for Scheme to implement FRP and Fran. In hindsight, I see how significant an undertaking this project was, and it probably was not a reasonable goal to achieve within one term. However, I did make progress while writing an interpreter for Scheme before deciding against the idea. I've also included source files for the interpreter project. This was implemented based on the [Write You a Scheme](https://wespiser.com/writings/wyas/00_overview.html) book.
