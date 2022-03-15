# IO Mocking

This exercise lets the students implement a simple I/O program that parses and processes its input and computes the average of a certain part of the input.
Instructions can be found [here](./instructions.pdf), exercise H12.2.

We test IO programs by replacing the `Prelude`, `System.IO`, and `System.Random` modules by mocked versions of those modules.
Those modules use a fake version of `RealWorld` where the file system, for example, is modeled as a map from file names to `Text`.
This lets us observe IO actions in student submissions and test just as any other piece of code.

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

## Acknowledgments

The inital mocking framework was written by [Manuel Eberl](http://pruvisto.org) at TU Munich.
