default:
  just --list

build:
  cargo build --bin compiler 

test-runner test_suite="": build
  cd compiler/ && deno run -A test/runner.ts {{test_suite}}

build-examples:
  mkdir -p playground/static
  deno run -A playground/build-examples.ts

run-examples: build build-examples
  cd compiler/ && deno run -A test/run-examples.ts

run-importer +args="-folder testpkg":
  cd importer/ && go run importer.go {{args}}

update-packages:
  deno run -A importer/update-packages.ts

serve-playground:
  python3 -m http.server 8888 --directory playground/static

playground-js: build-examples
  cp playground/style.css playground/static
  cd playground && npx esbuild --bundle wasm-index.js --outfile=static/bundle.js

playground-wasm mode="--debug":
  #!/usr/bin/env bash
  cd wasm/
  wasm-pack build --target web {{mode}}
  cp -R pkg ../playground/static
  rm ../playground/static/pkg/.gitignore

playground-prod: playground-js
  #!/usr/bin/env bash
  just playground-wasm --release
  cd playground/static
  cat bundle.js | npx esbuild --minify > prod.js
  mv prod.js bundle.js

watch +command:
  watchexec -e rs,md,ts,brg,go,html,js,css,eta "just {{command}}"

deploy-playground destination: playground-prod
  cp -R playground/static/* {{destination}}

init-project folder:
  #!/usr/bin/env bash
  set -euo pipefail

  DEST={{folder}}
  ROOT="."
  MAIN="main.brg"

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
  cp -R $ROOT/std $DEST

  echo "
  module borgo_test
  go 1.19
  " > $DEST/go.mod

  echo "
  use fmt;

  fn main() {
    fmt.Println(\"Hello world!\");
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
