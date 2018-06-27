# Protocol Buffers test suite

This package is used for testing `hprotoc` and the code that it generates.

To create a new test first add proto file(s) to the proto folder. Second, run the following Make task: `make codegen`. This will run `hprotoc` on all the proto files and update this package with the new code that is generated. Fourth, write tests in the included test suite. New test modules should be added to the `other-modules` section of the cabal file. Modules added to `other-modules` will not be overwritten by subsequent runs of `make codegen`.

## Travis CI integration

*TODO*
