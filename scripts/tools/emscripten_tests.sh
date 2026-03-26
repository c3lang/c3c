#!/usr/bin/env bash
# Usage: ./emscripten_tests.sh <path_to_c3c_binary>

if [ $# -lt 1 ]; then
    echo "Usage: ./emscripten_tests.sh <path_to_c3c_binary>"
    exit 1
fi

set -ex

# --- Setup Paths & Environment ---

# Resolve Script and Real Root Directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REAL_ROOT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"

if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    C3C_BIN="$(cygpath -m "$(realpath "$1")")"
else
    C3C_BIN="$(realpath "$1")"
fi

if ! command -v node &> /dev/null; then
    echo "::error::Node.js is not installed but required for Emscripten testing."
    exit 1
fi

if ! command -v emcc &> /dev/null; then
    echo "::error::Emscripten (emcc) is not in PATH but required for Emscripten testing."
    exit 1
fi

echo ">>> Running Emscripten Tests using C3C at: $C3C_BIN"

# --- Create Disposable Workspace ---

WORK_DIR=$(mktemp -d 2>/dev/null || mktemp -d -t 'c3_emscripten_tests')
echo ">>> Setting up workspace in: $WORK_DIR"

cleanup() {
    echo ">>> Cleaning up..."
    cd "$REAL_ROOT_DIR" || cd ..
    rm -rf "$WORK_DIR"
}
trap cleanup EXIT

# Copy necessary test data to the temp directory
cp -r "$REAL_ROOT_DIR/resources" "$WORK_DIR/resources"
cp -r "$REAL_ROOT_DIR/test"      "$WORK_DIR/test"

ROOT_DIR="$WORK_DIR"

# --- Tests ---

run_unit_tests() {
    echo "--- Running Emscripten Unit Tests ---"
    cd "$ROOT_DIR/test"

    # Compile the internal unit tests natively to JS output and execute using Node
    "$C3C_BIN" compile-test unit -O1 --target emscripten -o unit_tests.js
    node unit_tests.js
}

run_examples() {
    echo "--- Running Pure Computational Examples ---"
    cd "$ROOT_DIR/resources"

    # We manually compile these and run them in node, replacing compile-run functionality.
    "$C3C_BIN" compile examples/base64.c3 --target emscripten -o base64.js
    node base64.js

    "$C3C_BIN" compile examples/binarydigits.c3 --target emscripten -o binarydigits.js
    node binarydigits.js

    "$C3C_BIN" compile examples/mandelbrot.c3 --target emscripten -o mandelbrot.js
    node mandelbrot.js
    
    "$C3C_BIN" compile examples/factorial_macro.c3 --target emscripten -o factorial.js
    node factorial.js

    "$C3C_BIN" compile examples/fannkuch-redux.c3 --target emscripten -o fannkuch.js
    node fannkuch.js 8

    "$C3C_BIN" compile examples/spectralnorm.c3 --target emscripten -o spectralnorm.js
    node spectralnorm.js 100

    "$C3C_BIN" compile examples/nbodies.c3 --target emscripten -o nbodies.js
    node nbodies.js 1000

    "$C3C_BIN" compile examples/plus_minus.c3 --target emscripten -o plus_minus.js
    echo "10 + 20 - 5" | node plus_minus.js

    "$C3C_BIN" compile examples/fasta.c3 --target emscripten -o fasta.js
    node fasta.js 100

    "$C3C_BIN" compile examples/args.c3 --target emscripten -o args.js
    node args.js -- foo -bar "baz baz"
}

run_wasm_bare() {
    echo "--- Running BARE WebAssembly Compile Check ---"
    cd "$ROOT_DIR/resources/testfragments"
    
    # Check that generic Wasm32 still compiles without OS environments
    "$C3C_BIN" compile --target wasm32 -g0 --no-entry -Os wasm4.c3 -o wasm4_test.wasm
}

# --- Execution ---
run_unit_tests
run_examples
run_wasm_bare
