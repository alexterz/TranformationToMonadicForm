# TransformationToMonadicForm

*Implement a tool to facilitate the use of impure code in Haskell*

The purpose of this tool is to transform all the functions of a program to
monadic form of the type: `m(a -> m b)`, where a, b can also be functions of the 
same type, creating nested functions like the following: `(a -> m (b -> m1 c))`. 
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

We implement an interface to check the compatibility of the original and 
transformated programs. There is a pure and impure version of the platform,
that is related to the ability of using or not monads as arguments of a 
function. You can start by accessing the appropriate path (/puretransf 
or /tranf), and run the following commands:

`stack build`
`stack run`


# Running the tests

You can load the input file, by running:

`:l name`

then the tool will automatically generate 3 files. The first,named `nameOutput.hs`
corresponds to the Original Haskell code, the second is an auxiliary file, 
named `nameMConvert.hs` and the third is the transformated file, named 
`nameTransfOutput.hs`. Afterwards, it will compile and run them, showing 
the results in the screen.

*Notes:
1) `name`, has no file extension
2) The input file has the follow format:
   a) Every function, has it's own type annotation that ends with a semicolon `;`
   b) Every definition of a specific function ends with a semicolon,
      except of the last, which ends up with a newline character `\n`
   c) It contains a `result` function (with a not function type) that will be
      passed to the `main`, of `nameOutput.hs` and `nameTransfOutput.hs`



## Background

`extensible-effects` is based on the work of
[Extensible Effects: An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/).

