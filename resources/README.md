# Resources

## Programming Exercises

Here you can find a selected list of exercises
ordered by the knowledge required to solve the tasks at hand.
Exercise instructions are linked in the `README` files
of the corresponding directories.

1. [esperanto](esperanto): translating numbers to their corrsponding string representation in Esperanto
   - Requirements: strings and integer operations and recursion on integers
   - Challenge: code-golfing
1. [sudoku](sudoku): backtracking based Sudoku solver for `n x n`-sized grids
   - Requirements: lists
   - Challenge: optimisation
1. [cuboid](cuboid): challenge to optimally decompose a cuboid into cubes
   - Requirements: lists
   - Challenge: mathematics, optimisation
1. [spelling\_correction](spelling_correction): implementing a spelling correction algorithm based on edit distances
   - Requirements: lists, simple typeclass constraints (e.g. `Eq`, `Ord`)
   - Challenge: optimisation
1. [synthesiser](synthesiser): a music synthesiser with effects
   - Requirements: lists and higher-order functions
   - Challenge: creativity
1. [resolution](resolution): a resolution prover with certificates for propositional logic 
   - Requirements: algebraic datatypes
   - Challenge: optimisation
1. [sat\_solver](sat_solver): a simple SAT-solver for propositional logic 
   - Requirements: algebraic datatypes
   - Challenge: optimisation
1. [uno](uno): a UNO console game framework
   - Requirements: algebraic datatypes
   - Challenge: software engineering
1. [turtle\_graphics](turtle_graphics): a turtle graphics tool based on L-systems
   - Requirements: IO and algebraic datatypes
   - Challenge: creativity

## Game Simulations

TODO: to be published

When offering exercises in which students implement strategies for games,
one faces the challenge of having to run each student's submissions against every other submissions.
We will provide a framework for this purpose, organising this simulation task.
The framework continuously accepts new submissions and accordingly updates the results for each game and student.
The generated score data can then easily be displayed, for example, on a tournament website.

We will also provide a sample game &ndash; [Chain Reaction](https://brilliant.org/wiki/chain-reaction-game/) &ndash;
that can be used together with the framework.
An example website for this setup can be found [here](https://vmnipkow16.in.tum.de/christmas2020/).

## IO Mocking

In [io\_mocking](io_mocking),
you can find a mocking framework for Haskell's `IO` type that
lets you observe IO actions in student submissions and test them just as any other piece of code.

## Check Your Proof (CYP)

Check Your Proof is a verifier for proofs about Haskell-like programs.
Please refer to the [paper](https://github.com/kappelmann/engaging-large-scale-functional-programming/releases/download/pdf/engaging_fp_education.pdf)
for more information.
You can find the source of CYP including some proof examples [here](https://github.com/lukasstevens/cyp)
and an exemplary integration of automated CYP proof checking in Tasty in [cyp\_integration](cyp_integration).

## Programming Contest

In [contest](contest),
you can find a simple application to run an
[ACM-ICPC-like programming contest](https://en.wikipedia.org/wiki/International_Collegiate_Programming_Contest).

