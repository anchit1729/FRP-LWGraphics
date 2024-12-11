# FRP-LWGraphics

Implementations of the Functional Reactive Programming (FRP) Domain-Specific Language (DSL) in Haskell and Racket.

This implementation follows the stream-based approach towards defining FRP constructs as outlined in the paper "Functional Reactive Programming from First Principles" [(paper link)](https://doi.org/10.1145/349299.349331). It builds this FRP implementation on top of a lightweight graphics library/wrapper that handles interfacing with OpenGL, based on [these lecture notes](http://www.cs.yale.edu/homes/hudak/SOE/powerpoi.htm) by Paul Hudak that provide a more up-to-date description of FRP for reactive graphics and animations.

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

## Important Features

## Known Limitations

## Performance

## Alternative Approaches

## Future Work/Applications
