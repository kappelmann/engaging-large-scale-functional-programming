# IO Mocking

We test IO programs by replacing the `Prelude`, `System.IO`, and `System.Random` modules by mocked versions of those modules.
Those modules use a fake version of `RealWorld` where the file system, for example, is modeled as a map from file names to `Text`.
This lets us observe IO actions in student submissions and test just as any other piece of code.

This is the minimal example that was presented in the paper.
For a more involved example see `../stocks`.

## Requirements

Tests are run using [stack](https://docs.haskellstack.org/en/stable/README/).

## Running Tests

The cabal file `test.cabal` defines an executable test.
You can run the test with `stack run test`.

## Acknowledgments

The inital mocking framework was written by [Manuel Eberl](http://pruvisto.org) at TU Munich.
