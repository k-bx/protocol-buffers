#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROTO_PATH="$DIR/proto"
PROTO_PREFIX="HSCodeGen"
HASKELL_SRC_PATH="$DIR/src"

stack exec hprotoc -- \
  --haskell_out=$HASKELL_SRC_PATH \
  --proto_path=$PROTO_PATH \
  --prefix=$PROTO_PREFIX \
  --unknown_fields `find $PROTO_PATH -type f | egrep '\.proto$'`
