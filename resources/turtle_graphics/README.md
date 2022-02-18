# Turtle Graphics

This exercise lets students implement a
turtle graphics tool based on L-systems.
Instructions can be found [here](https://www21.in.tum.de/teaching/fpv/WS20/assets/ex10.pdf), exercise H10.1,
and some *masterpieces* by our students
can be found [here](https://www21.in.tum.de/teaching/fpv/WS20/wettbewerb.html#sch%C3%B6nheitswettbewerb-semester-closing).

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

