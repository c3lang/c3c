#!/usr/bin/env bash
# Usage: ./ci_tests.sh <path_to_c3c_binary> [optional_os_mode_override]

if [ $# -lt 1 ]; then
    echo "Usage: ./ci_tests.sh <path_to_c3c_binary> [os_mode]"
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

# Detect OS
SYSTEM_NAME="$(uname -s)"
if [ -n "$2" ]; then
    OS_MODE="$2"
else
    case "$SYSTEM_NAME" in
        CYGWIN*|MINGW*|MSYS*) OS_MODE="windows" ;;
        Darwin*)              OS_MODE="mac" ;;
        Linux*)               OS_MODE="linux" ;;
        *BSD)                 OS_MODE="bsd" ;;
        *)                    OS_MODE="linux" ;;
    esac
fi

echo ">>> Running CI Tests using C3C at: $C3C_BIN"
echo ">>> OS Mode: $OS_MODE (Detected System: $SYSTEM_NAME)"

# --- Create Disposable Workspace ---

# Create temp directory
WORK_DIR=$(mktemp -d 2>/dev/null || mktemp -d -t 'c3_ci_tests')
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

# ROOT_DIR points to the temp workspace.
ROOT_DIR="$WORK_DIR"

# Move to the temp resources dir to match original script behavior
cd "$ROOT_DIR/resources"

# --- Tests ---

run_examples() {
    echo "--- Running Standard Examples ---"
    "$C3C_BIN" compile-only -vv examples/base64.c3 --target linux-x64
    "$C3C_BIN" compile -vv examples/base64.c3
    "$C3C_BIN" compile -vv examples/binarydigits.c3
    "$C3C_BIN" compile -vv examples/brainfk.c3
    "$C3C_BIN" compile -vv examples/factorial_macro.c3
    "$C3C_BIN" compile -vv examples/fasta.c3
    "$C3C_BIN" compile -vv examples/gameoflife.c3
    "$C3C_BIN" compile -vv examples/hash.c3
    "$C3C_BIN" compile-only -vv examples/levenshtein.c3
    "$C3C_BIN" compile -vv examples/load_world.c3
    "$C3C_BIN" compile-only -vv examples/map.c3
    "$C3C_BIN" compile -vv examples/mandelbrot.c3
    "$C3C_BIN" compile -vv examples/plus_minus.c3
    "$C3C_BIN" compile -vv examples/nbodies.c3
    "$C3C_BIN" compile -vv examples/spectralnorm.c3
    "$C3C_BIN" compile -vv examples/swap.c3
    "$C3C_BIN" compile -vv examples/contextfree/boolerr.c3
    "$C3C_BIN" compile -vv examples/contextfree/dynscope.c3
    "$C3C_BIN" compile -vv examples/contextfree/guess_number.c3
    "$C3C_BIN" compile -vv examples/contextfree/multi.c3
    "$C3C_BIN" compile -vv examples/contextfree/cleanup.c3
    
    "$C3C_BIN" compile-run -vv examples/hello_world_many.c3
    "$C3C_BIN" compile-run -vv examples/time.c3
    "$C3C_BIN" compile-run -vv examples/fannkuch-redux.c3
    "$C3C_BIN" compile-run -vv examples/contextfree/boolerr.c3
    "$C3C_BIN" compile-run -vv examples/load_world.c3
    "$C3C_BIN" compile-run -vv examples/process.c3
    "$C3C_BIN" compile-run -vv examples/ls.c3
    "$C3C_BIN" compile-run -vv examples/args.c3 -- foo -bar "baz baz"

    if [[ "$OS_MODE" == "linux" ]]; then
        "$C3C_BIN" compile-run --linker=builtin linux_stack.c3 || echo "Warning: linux_stack builtin linker skipped"
        "$C3C_BIN" compile-run linux_stack.c3
    fi
    
    "$C3C_BIN" compile --no-entry --test -g --threads 1 --target macos-x64 examples/constants.c3
}

run_cli_tests() {
    echo "--- Running CLI Tests (init/vendor) ---"
    
    # Test init
    "$C3C_BIN" init-lib mylib
    "$C3C_BIN" init myproject
    rm -rf mylib.c3l myproject

    # Test vendor-fetch
    if [ -n "$SKIP_NETWORK_TESTS" ]; then
        echo "Skipping vendor-fetch (network tests disabled)"
    else
        echo "Testing vendor-fetch..."
        cd "$ROOT_DIR/resources"
        "$C3C_BIN" vendor-fetch raylib

        if [ -f "/etc/alpine-release" ] || [[ "$SYSTEM_NAME" == "OpenBSD" ]] || [[ "$SYSTEM_NAME" == "NetBSD" ]] || [[ "$OS_MODE" == "android" ]]; then
            echo "Skipping raylib_arkanoid (vendor raylib doesn't support this platform)"
            return
        fi
        "$C3C_BIN" compile --lib raylib --print-linking examples/raylib/raylib_arkanoid.c3
    fi
}

run_dynlib_tests() {
    echo "--- Running Dynamic Lib Tests ---"
    # Skip openbsd and android
    if [[ "$SYSTEM_NAME" == *"OpenBSD"* ]] || [[ "$OS_MODE" == "android" ]]; then return; fi

    cd "$ROOT_DIR/resources/examples/dynlib-test"
    "$C3C_BIN" -vv dynamic-lib add.c3

    if [[ "$OS_MODE" == "windows" ]]; then
        "$C3C_BIN" -vv compile-run test.c3 -l ./add.lib
    elif [[ "$OS_MODE" == "mac" ]]; then
        "$C3C_BIN" -vv compile-run test.c3 -l ./add.dylib
    else 
        if [ -f add.so ]; then mv add.so libadd.so; fi
        cc test.c -L. -ladd -Wl,-rpath=.
        ./a.out
        "$C3C_BIN" compile-run test.c3 -L . -l add -z -Wl,-rpath=.
    fi
}

run_staticlib_tests() {
    echo "--- Running Static Lib Tests ---"
    cd "$ROOT_DIR/resources/examples/staticlib-test"
    
    if [[ "$OS_MODE" == "windows" ]]; then
        "$C3C_BIN" -vv static-lib add.c3
        "$C3C_BIN" -vv compile-run test.c3 -l ./add.lib
    else
        "$C3C_BIN" -vv static-lib add.c3 -o libadd
        if [[ "$SYSTEM_NAME" == *"NetBSD"* ]]; then ranlib libadd.a; fi

        OUTPUT_BIN="a.out"
        if [[ "$SYSTEM_NAME" == *"OpenBSD"* ]]; then
             cc test.c -L. -ladd -lexecinfo -lm -lpthread -o "$OUTPUT_BIN"
        elif [[ "$SYSTEM_NAME" == "Linux" ]]; then
             # Fix: Linux (and i mean specifically the docker container run) needs dl (for backtrace)
             # math and pthread linked manually for static libs
             cc test.c -L. -ladd -ldl -lm -lpthread -o "$OUTPUT_BIN"
        else
             # Mac / NetBSD
             cc test.c -L. -ladd -o "$OUTPUT_BIN"
        fi
        ./"$OUTPUT_BIN"
        "$C3C_BIN" compile-run test.c3 -L . -l add
    fi
}

run_testproject() {
    echo "--- Running Test Project ---"
    cd "$ROOT_DIR/resources/testproject"

    ARGS="--trust=full"
    
    if [[ "$OS_MODE" == "linux" || "$OS_MODE" == "mac" ]]; then
        ARGS="$ARGS --linker=builtin"

        if [ -f "/etc/alpine-release" ]; then
            ARGS="$ARGS --linux-libc=musl"
        fi
    fi

    "$C3C_BIN" run -vv $ARGS
    "$C3C_BIN" clean

    if [[ "$OS_MODE" == "windows" ]]; then
        echo "Running Test Project (hello_world_win32)..."
        "$C3C_BIN" -vv --emit-llvm run hello_world_win32 $ARGS
        "$C3C_BIN" clean
        "$C3C_BIN" -vv build hello_world_win32_lib $ARGS
    fi
}

run_wasm_compile() {
    echo "--- Running WASM Compile Check ---"
    cd "$ROOT_DIR/resources/testfragments"
    "$C3C_BIN" compile --target wasm32 -g0 --no-entry -Os wasm4.c3
}

run_unit_tests() {
    echo "--- Running Unit Tests ---"
    cd "$ROOT_DIR/test"

    UNIT_TEST_ARGS="-O1"
    if [[ "$OS_MODE" != "bsd" ]]; then
        UNIT_TEST_ARGS="$UNIT_TEST_ARGS -D SLOW_TESTS"
    fi
    "$C3C_BIN" compile-test unit $UNIT_TEST_ARGS

    echo "--- Running Test Suite Runner ---"
    "$C3C_BIN" compile-run -O1 src/test_suite_runner.c3 -- "$C3C_BIN" test_suite/ --no-terminal
}

# --- Execution ---
run_examples
run_cli_tests
run_dynlib_tests
run_staticlib_tests
run_testproject
run_wasm_compile
run_unit_tests
