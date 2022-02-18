# Esperanto

This exercise lets students implement a
function that translates numbers to their corresponding string representation in Esperanto.
Instructions can be found [here](instructions.pdf), exercise H1.2,
and some cleverly code-golfed submissions by our students
can be found [here](https://www21.in.tum.de/teaching/fpv/WS20/wettbewerb.html#week-1).

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

