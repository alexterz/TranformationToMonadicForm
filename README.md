# TransformationToMonadicForm

*Implement a tool to facilitate the use of impure code in Haskell*

The purpose of this tool is to transform all the functions of a program to
monadic form of the type: `a -> m b`, where a, b can also be functions of the 
same type, creating nested functions like the following: `a -> m (b -> m1 c)`. 
We would like to have the ability to compose the effects of nested/different
monads, so we chose to use the `extensible-effects` library. 

The main monad has been used is `Eff` from `Control.Eff`.
`Eff r a` is parameterized by the effect-list `r` and the monadic-result type
`a` similar to other monads.
It is the intention that all other monadic computations can be replaced by the
use of `Eff`.



# Getting Started

The programs have been run and tested with the stack tool (version 1.9.3),
GHC version 8.6.4

You can start by running the following command:
`stack exec --package extensible-effects ghci` 

# Running the tests

`ForTesting` module, contains some first order functions of the appropriate 
form, that are used as input to the higher order functions (map, zipWith, 
fold, etc.)

Each higher order function has been tested under its implementation.
You can run the corresponded `ti` to see the result.


## Background

`extensible-effects` is based on the work of
[Extensible Effects: An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/).

