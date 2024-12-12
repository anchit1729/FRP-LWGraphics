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

## Known Limitations

## Performance

## Alternative Approaches

## Future Work/Applications
