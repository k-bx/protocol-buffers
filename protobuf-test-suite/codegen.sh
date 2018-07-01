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
