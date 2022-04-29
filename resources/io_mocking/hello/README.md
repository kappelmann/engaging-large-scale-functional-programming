# IO Mocking

We test IO programs by replacing the `Prelude`, `System.IO`, and `System.Random` modules by mocked versions of those modules.
Those modules use a fake version of `RealWorld` where the file system, for example, is modeled as a map from file names to `Text`.
This lets us observe IO actions in student submissions and test just as any other piece of code.

TODO: Change README

## Acknowledgments

The inital mocking framework was written by [Manuel Eberl](http://pruvisto.org) at TU Munich.
