#!/usr/bin/env bash

set -euo pipefail

if [ $# != 1 ];
then
    echo "USAGE: $0 PROJECT_NAME"
    exit 1
fi

DEST=$1
ROOT="."
MAIN="app.brg"
RUNTIME="$ROOT/runtime"


if [ -d "$DEST" ];
then
    echo "Folder \"$DEST\" already exists."
    exit 1
fi

echo "Creating project folder: \"$DEST\""
mkdir -p $DEST

echo "Building compiler"
cargo build --bin compiler

echo "Copying files"
cp -R $RUNTIME/immutable $DEST
cp -R $RUNTIME/interpreter $DEST
cp -R $RUNTIME/runtime $DEST
cp -R $RUNTIME/std $DEST
cp $RUNTIME/go.mod $DEST
cp $RUNTIME/go.sum $DEST
cp $RUNTIME/entrypoint $DEST/main.go

echo "

fn borgo_main() {
  \"Hello world!\".inspect();
  ()
}
" > $DEST/$MAIN

# Copy compiler as well
cp $ROOT/target/debug/compiler $DEST/borgo

# cd $DEST
# ./borgo build && go run .

echo "

  Done! You can now edit $MAIN and run the compiler.

  cd $DEST
  ./borgo build && go run .

  Keep in mind a lot of stuff is broken. Have fun!
"
