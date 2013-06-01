# Stackist: a stack-based programming language and runtime

[![Build Status](https://secure.travis-ci.org/stevej/stackist.png?branch=master)](http://travis-ci.org/stevej/stackist)

`stackist` is a series of experiments in building a stack-based
programming language, starting with Manfred von Thun's Joy language.

What I have today:
* A basic interpreter for an arithmetic language.
* Literals
* Operators
* A basic stdandard library
* Quotations
* Most of the base combinators


What I'm working on next:
* Aggregates
* All combinators (primrec, linrec, binrec, fold, ifte)
* Critical: verify that my stack semantics agree with von Thun's.
* More stdlib (working down this list: http://www.latrobe.edu.au/phimvt/joy/html-manual.html)
* Named functions
* I/O
* stack operators (newstack, clear, stack, cons, swons, uncons, unswons)
* better interpretor error handling
* a parser for the concrete syntax

Future directions:
* Documenting a type system for Joy
* Can we encode Joy with a fragment of System F? (rank-2?)
* Type reconstructor applicable to all original Joy programs
* Retargetable compiler
* Compile to C
* Compile to LLVM IR
* Compile to Native code
* Applying Linear types to Joy
* Linear type reconstruction
