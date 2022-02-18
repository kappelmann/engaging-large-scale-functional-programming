# Resolution Prover

This exercise lets students implement a
resolution prover to propositional logic
that provides certificates (a series of resolution steps or a model) for its proofs.
Instructions can be found [here](https://www21.in.tum.de/teaching/fpv/WS20/assets/ex09.pdf), exercise H9.2,
and some optimisations submitted by our students
and further discussion can be found [here](https://www21.in.tum.de/teaching/fpv/WS20/wettbewerb.html#week-8).

## Requirements

Tests are run using [stack](https://docs.haskellstack.org/en/stable/README/).

## Setup

The executables specified in `test.cabal` expect the solution repository checked out in the `solution` subdirectory and the submission checked out in the `assignment` subdirectory.
Moreover, `test.cabal` provides an executable to test the template repository locally.
For this, it expects the template repository in the `template` subdirectory.

## Running Tests

Refer to `test.cabal` for detailed information about the targets and flags provided.

### Locally

You can run executables specified in `test.cabal` using `stack run <executableName>`.

### On Artemis

By default, Artemis runs `./run.sh -s` to execute the tests.
You can modify `run.sh` to adapt the build and test process.
