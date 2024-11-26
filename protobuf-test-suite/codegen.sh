#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROTO_PATH="$DIR/proto"
PROTO_PREFIX="HSCodeGen"
PROTO_LENSE_PREFIX="HSCodeGen.Lense"
HASKELL_SRC_PATH="$DIR/src"

stack exec hprotoc -- \
  --haskell_out=$HASKELL_SRC_PATH \
  --proto_path=$PROTO_PATH \
  --prefix=$PROTO_PREFIX \
  --json \
  --unknown_fields `find $PROTO_PATH -type f | egrep '\.proto$'`

stack exec hprotoc -- \
  --haskell_out=$HASKELL_SRC_PATH \
  --proto_path=$PROTO_PATH \
  --prefix=$PROTO_LENSE_PREFIX \
  --lenses \
  --json \
  --unknown_fields `find $PROTO_PATH -type f | egrep '\.proto$'`
