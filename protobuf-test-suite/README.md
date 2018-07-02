# Protocol Buffers test suite

This package is used for testing `hprotoc` and the code that it generates.

To create a new test first add proto file(s) to the proto folder. Second, run the following Make task: `make codegen`. This will run `hprotoc` on all the proto files and update this package with the new code that is generated. Fourth, write tests in the included test suite. Add any new test modules to the test suite `other-modules` section in `package.yaml` (this package uses [hpack](https://github.com/sol/hpack) to generate a cabal file).

## Travis CI integration

The `make test` task is intended to be used in the `.travis.yaml` `script` section:

```yaml
script:
  - make test
```
