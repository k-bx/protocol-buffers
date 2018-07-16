build:
	stack build
.PHONY: build

codegen:
	protobuf-test-suite/codegen.sh

test: build codegen
	stack test protobuf-test-suite

clean:
	stack clean
	rm -rf protobuf-test-suite/src/HSCodeGen
.PHONY: clean
