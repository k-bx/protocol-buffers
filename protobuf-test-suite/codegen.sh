#!/bin/bash

cd `dirname $0`
PROTO_PATH="proto"
PROTO_PREFIX="HSCodeGen"
HASKELL_SRC_PATH="src/"

stack exec hprotoc -- \
  --haskell_out=$HASKELL_SRC_PATH \
  --proto_path=$PROTO_PATH \
  --prefix=$PROTO_PREFIX \
  --unknown_fields `find $PROTO_PATH  -type f | egrep '\.proto$'`

cd "$HASKELL_SRC_PATH"
EXPOSED_MODULES=`find * -print | egrep '\.hs$' | egrep "$PROTO_PREFIX" | cut -f 1 -d '.' | tr / .`
cd -

display_modules () {
  for i in ${1}; do
    printf "\n%"$2"s$i";
  done
}

CABAL_FILE="protobuf-test-suite.cabal"

OTHER_MODULES=""
[ -f "$CABAL_FILE" ] && OTHER_MODULES=`egrep 'other-modules' "$CABAL_FILE" -A10000`

cat << EOF > "$CABAL_FILE"
name:                protobuf-test-suite
version:             0.1.0.0
synopsis:            Protocol buffers auto-generated test suite package for testing protobuf codegen and serdes
author:              Nobody
maintainer:          nobody@example.com
build-type:          Simple
cabal-version:       >= 1.8
library
  exposed-modules:`display_modules "$EXPOSED_MODULES" 8`
  build-depends:        base
                      , filepath
                      , process
                      , protocol-buffers
                      , protocol-buffers-descriptor
  hs-source-dirs:       src
  ghc-options:          -O1

test-suite protobuf-tests
  build-depends:   protocol-buffers,
                   protocol-buffers-descriptor,
                   protobuf-test-suite
  type: exitcode-stdio-1.0

  main-is:        Main.hs
  hs-source-dirs: test/src

  build-depends: base >= 4.7.0 && < 5
               , text
               , containers
               , HUnit
               , bytestring
$OTHER_MODULES
EOF
