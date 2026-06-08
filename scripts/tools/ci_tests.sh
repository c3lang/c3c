#!/usr/bin/env bash
# Usage: ./ci_tests.sh <path_to_c3c_binary> [optional_os_mode_override]

# Toggle to show output of successful parallel tasks (true: show, false: hide)
SHOW_SUCCESS_LOGS="${SHOW_SUCCESS_LOGS:-true}"

if [ $# -lt 1 ]; then
    echo "Usage: ./ci_tests.sh <path_to_c3c_binary> [os_mode]"
    exit 1
fi

set -e

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

# ROOT_DIR points to the actual source repository
ROOT_DIR="$REAL_ROOT_DIR"

# --- Tests ---

# Helper to run c3c with the correct workspace isolation
run_c3c() {
    "$C3C_BIN" --output-dir "$MY_WORK_DIR" --build-dir "$MY_WORK_DIR" --obj-out "$MY_WORK_DIR" "$@"
}

run_examples() {
    local MY_WORK_DIR="$WORK_DIR/examples"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Standard Examples ---"
    cd "$ROOT_DIR/resources"

    run_c3c compile examples/base64.c3
    run_c3c compile examples/binarydigits.c3
    run_c3c compile examples/brainfk.c3
    run_c3c compile examples/factorial_macro.c3
    run_c3c compile examples/fasta.c3
    run_c3c compile examples/gameoflife.c3
    run_c3c compile examples/hash.c3
    run_c3c compile examples/http_server.c3
    run_c3c compile-only examples/levenshtein.c3
    run_c3c compile examples/load_world.c3
    run_c3c compile-only examples/map.c3
    run_c3c compile examples/mandelbrot.c3
    run_c3c compile examples/plus_minus.c3
    run_c3c compile examples/nbodies.c3
    run_c3c compile examples/spectralnorm.c3
    run_c3c compile examples/swap.c3
    run_c3c compile examples/contextfree/boolerr.c3
    run_c3c compile examples/contextfree/dynscope.c3
    run_c3c compile examples/contextfree/guess_number.c3
    run_c3c compile examples/contextfree/multi.c3
    run_c3c compile examples/contextfree/cleanup.c3

    run_c3c compile-run examples/hello_world_many.c3
    run_c3c compile-run examples/time.c3
    run_c3c compile-run examples/fannkuch-redux.c3
    run_c3c compile-run examples/contextfree/boolerr.c3
    run_c3c compile-run examples/load_world.c3
    run_c3c compile-run examples/process.c3
    run_c3c compile-run examples/ls.c3
    run_c3c compile-run examples/args.c3 -- foo -bar "baz baz"

    if [[ "$OS_MODE" == "linux" ]]; then
        run_c3c compile-run --linker=builtin linux_stack.c3 || echo "Warning: linux_stack builtin linker skipped"
        run_c3c compile-run linux_stack.c3
    fi

    run_c3c compile --no-entry --test -g --threads 1 --target macos-x64 examples/constants.c3
}

run_cli_tests() {
    local MY_WORK_DIR="$WORK_DIR/cli"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running CLI Tests (init/vendor) ---"

    # Test init
    (
        cd "$MY_WORK_DIR"
        "$C3C_BIN" init-lib mylib
        "$C3C_BIN" init myproject
        (cd myproject && "$C3C_BIN" benchmark myproject --suppress-run)
        rm -rf mylib.c3l myproject
    )

    # Test vendor-fetch
    if [ -n "$SKIP_NETWORK_TESTS" ]; then
        echo "Skipping vendor-fetch (network tests disabled)"
    else
        echo "Testing vendor-fetch..."
        cd "$MY_WORK_DIR"
        mkdir -p lib
        if ! "$C3C_BIN" --stdlib "$ROOT_DIR/lib" vendor-fetch raylib6; then
            echo "::warning::vendor-fetch failed. Skipping dependent tests."
            return
        fi

        if [ -f "/etc/alpine-release" ] || [[ "$SYSTEM_NAME" == "OpenBSD" ]] || [[ "$SYSTEM_NAME" == "NetBSD" ]] || [[ "$OS_MODE" == "android" ]]; then
            echo "::warning::Skipping raylib_arkanoid (vendor raylib doesn't support this platform)"
            return
        fi
        run_c3c compile --stdlib "$ROOT_DIR/lib" --libdir "$MY_WORK_DIR" --lib raylib6 --print-linking "$ROOT_DIR/resources/examples/raylib/raylib_arkanoid.c3"
    fi
}

run_dynlib_tests() {
    local MY_WORK_DIR="$WORK_DIR/dynlib"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Dynamic Lib Tests ---"
    # Skip openbsd and android
    if [[ "$SYSTEM_NAME" == *"OpenBSD"* ]] || [[ "$OS_MODE" == "android" ]]; then return; fi

    cd "$MY_WORK_DIR"
    run_c3c -vv dynamic-lib "$ROOT_DIR/resources/examples/dynlib-test/add.c3" -o add

    if [[ "$OS_MODE" == "windows" ]]; then
        run_c3c -vv compile-run "$ROOT_DIR/resources/examples/dynlib-test/test.c3" -l "add.lib"
    elif [[ "$OS_MODE" == "mac" ]]; then
        run_c3c -vv compile-run "$ROOT_DIR/resources/examples/dynlib-test/test.c3" -l "add.dylib"
    else
        if [ -f "add.so" ]; then mv "add.so" "libadd.so"; fi
        cc "$ROOT_DIR/resources/examples/dynlib-test/test.c" -L. -ladd -Wl,-rpath=. -o a.out
        ./a.out
        run_c3c compile-run "$ROOT_DIR/resources/examples/dynlib-test/test.c3" -L . -l add -z -Wl,-rpath=.
    fi
}

run_staticlib_tests() {
    local MY_WORK_DIR="$WORK_DIR/staticlib"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Static Lib Tests ---"
    cd "$MY_WORK_DIR"

    if [[ "$OS_MODE" == "windows" ]]; then
        run_c3c -vv static-lib "$ROOT_DIR/resources/examples/staticlib-test/add.c3" -o add
        run_c3c -vv compile-run "$ROOT_DIR/resources/examples/staticlib-test/test.c3" -l "add.lib"
    else
        run_c3c -vv static-lib "$ROOT_DIR/resources/examples/staticlib-test/add.c3" -o libadd
        if [[ "$SYSTEM_NAME" == *"NetBSD"* ]]; then ranlib libadd.a; fi

        if [[ "$SYSTEM_NAME" == *"OpenBSD"* ]]; then
             cc "$ROOT_DIR/resources/examples/staticlib-test/test.c" -L. -ladd -lexecinfo -lm -lpthread -o a.out
        elif [[ "$SYSTEM_NAME" == "Linux" ]]; then
             cc "$ROOT_DIR/resources/examples/staticlib-test/test.c" -L. -ladd -ldl -lm -lpthread -o a.out
        else
             # Mac / NetBSD
             cc "$ROOT_DIR/resources/examples/staticlib-test/test.c" -L. -ladd -o a.out
        fi
        ./a.out
        run_c3c compile-run "$ROOT_DIR/resources/examples/staticlib-test/test.c3" -L . -l add
    fi
}

run_testproject() {
    local MY_WORK_DIR="$WORK_DIR/testproject"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Test Project ---"
    cd "$ROOT_DIR/resources/testproject"

    ARGS="--trust=full"

    if [[ "$OS_MODE" == "linux" || "$OS_MODE" == "mac" ]]; then
        ARGS="$ARGS --linker=builtin"

        if [ -f "/etc/alpine-release" ]; then
            ARGS="$ARGS --linux-libc=musl"
        fi
    fi

    run_c3c run -vv $ARGS
    run_c3c clean

    if [[ "$OS_MODE" == "windows" ]]; then
        echo "Running Test Project (hello_world_win32)..."
        run_c3c -vv --emit-llvm run hello_world_win32 $ARGS
        run_c3c clean
        run_c3c -vv build hello_world_win32_lib $ARGS
    fi
}

run_wasm_compile() {
    local MY_WORK_DIR="$WORK_DIR/wasm"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running WASM Compile Check ---"
    cd "$ROOT_DIR/resources/testfragments"
    run_c3c compile --target wasm32 -g0 --no-entry -Os wasm4.c3
}

run_bsd_cross_compile() {
    # Skip if running in a container, Nix, or non-standard targets (android, native bsd) to avoid redundant uncached downloads
    if [ -f /etc/alpine-release ] || [ -f /.dockerenv ] || [ -n "$NIX_BUILD_TOP" ]; then
        echo "Skipping BSD cross-compilation test in container/Nix environment."
        return
    fi
    if [[ "$OS_MODE" != "linux" && "$OS_MODE" != "mac" && "$OS_MODE" != "windows" ]]; then
        echo "Skipping BSD cross-compilation test on target $OS_MODE."
        return
    fi

    local MY_WORK_DIR="$WORK_DIR/bsd_cross"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running BSD Cross-Compile Check ---"

    local SYSROOT_DIR="$REAL_ROOT_DIR/.cache/freebsd-sysroot"
    local CACHE_TXZ="$REAL_ROOT_DIR/.cache/base.txz"
    if [ ! -d "$SYSROOT_DIR" ]; then
        echo "Preparing FreeBSD sysroot..."
        mkdir -p "$REAL_ROOT_DIR/.cache"
        if [ ! -f "$CACHE_TXZ" ]; then
            echo "Downloading FreeBSD base.txz..."
            if command -v wget &> /dev/null; then
                wget -q -O "$CACHE_TXZ" https://download.freebsd.org/releases/amd64/15.0-RELEASE/base.txz
            else
                curl -sSL -o "$CACHE_TXZ" https://download.freebsd.org/releases/amd64/15.0-RELEASE/base.txz
            fi
        fi
        echo "Extracting sysroot files to .cache/freebsd-sysroot..."
        mkdir -p "$SYSROOT_DIR"
        if [[ "$OS_MODE" == "windows" ]]; then
            # Windows tar cannot create POSIX symlinks and exits with code 2 for those failures.
            # We tolerate this: the actual .a/.o files we need are still extracted correctly.
            # We redirect stderr to /dev/null to avoid polluting the GitHub Actions log with symlink warnings.
            tar -xf "$CACHE_TXZ" -C "$SYSROOT_DIR" ./usr/lib ./lib ./libexec/ld-elf.so.1 2> /dev/null || true
        else
            tar -xf "$CACHE_TXZ" -C "$SYSROOT_DIR" ./usr/lib ./lib ./libexec/ld-elf.so.1
        fi
        # Verify the critical linker inputs were actually extracted regardless of symlink failures.
        if [ ! -f "$SYSROOT_DIR/usr/lib/libc.a" ] || [ ! -f "$SYSROOT_DIR/usr/lib/crt1.o" ]; then
            echo "ERROR: FreeBSD sysroot extraction failed - libc.a or crt1.o missing from $SYSROOT_DIR"
            exit 1
        fi
    fi

    cd "$MY_WORK_DIR"
    echo 'import std; fn void main() { io::printn("Hello BSD"); }' > main.c3

    run_c3c compile --target freebsd-x64 --bsd-sysroot "$SYSROOT_DIR" main.c3
    echo "BSD cross-compilation successfully linked executable."
}

run_http_server_tests() {
    local MY_WORK_DIR="$WORK_DIR/http"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running HTTP Server Integration Tests ---"

    if [ -n "$SKIP_NETWORK_TESTS" ]; then
        echo "Skipping HTTP server request tests (network tests disabled)"
        return
    fi

    if ! command -v curl &> /dev/null; then
        echo "::warning::curl not found, skipping HTTP server integration tests"
        return
    fi

    cd "$ROOT_DIR/resources/examples"
    run_c3c compile -O1 http_server.c3

    OUTPUT_BIN="$MY_WORK_DIR/http_server"
    if [[ "$OS_MODE" == "windows" ]]; then
        OUTPUT_BIN="$MY_WORK_DIR/http_server.exe"
    fi

    PORT=$(( 8085 + $RANDOM % 10000 ))
    echo "Starting server on port $PORT..."
    "$OUTPUT_BIN" -p $PORT -r "$ROOT_DIR/resources/examples" &
    SERVER_PID=$!

    sleep 1

    # Test root path (directory listing)
    echo "Testing GET /"
    HTTP_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://127.0.0.1:$PORT/")
    if [ "$HTTP_STATUS" != "200" ]; then
        echo "::error::HTTP GET / failed with status $HTTP_STATUS."
        kill $SERVER_PID 2>/dev/null || true
        exit 1
    fi

    # Test served file
    echo "Testing GET /http_server.c3"
    HTTP_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://127.0.0.1:$PORT/http_server.c3")
    if [ "$HTTP_STATUS" != "200" ]; then
        echo "::error::HTTP GET /http_server.c3 failed with status $HTTP_STATUS."
        kill $SERVER_PID 2>/dev/null || true
        exit 1
    fi

    # Test missing file (404 expected)
    echo "Testing 404 for invalid path"
    HTTP_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://127.0.0.1:$PORT/does_not_exist_404_test")
    if [ "$HTTP_STATUS" != "404" ]; then
        echo "::error::HTTP GET /does_not_exist_404_test expected 404, but got $HTTP_STATUS."
        kill $SERVER_PID 2>/dev/null || true
        exit 1
    fi

    echo "HTTP Server Integration Tests passed."
    kill $SERVER_PID 2>/dev/null || true
}

run_unit_tests() {
    local MY_WORK_DIR="$WORK_DIR/unit"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Unit Tests ---"
    cd "$ROOT_DIR/test"

    UNIT_TEST_ARGS="-O1"
    if [[ "$OS_MODE" != "bsd" ]]; then
        UNIT_TEST_ARGS="$UNIT_TEST_ARGS -D SLOW_TESTS -D RUN_PROCESS_TESTS"
    fi
    run_c3c compile-test unit $UNIT_TEST_ARGS

    echo "--- Running Test Suite Runner ---"
    (
        cd "$MY_WORK_DIR"
        run_c3c compile-run -O1 "$ROOT_DIR/test/src/test_suite_runner.c3" -- "$C3C_BIN" "$ROOT_DIR/test/test_suite/" --no-terminal
    )
}

# --- Execution ---

PIDS=()

# Function to run a suite and capture its output in the background
run_parallel() {
    local name=$1
    local func=$2
    local MY_WORK_DIR="$WORK_DIR/$name"
    local log="$WORK_DIR/$name.log"

    (
        set +e
        # Inner subshell handles the actual test execution with 'set -e'
        ( set -e; $func ) > "$log" 2>&1
        local status=$?

        if [ $status -eq 0 ]; then
            echo "SUCCESS: $name"
            if [ "$SHOW_SUCCESS_LOGS" = "true" ]; then
                echo "--------------------------------------------------------------------------------"
                cat "$log"
                echo "--------------------------------------------------------------------------------"
            fi
        else
            echo "FAILED: $name (see log below)"
            echo "--------------------------------------------------------------------------------"
            cat "$log"
            echo "--------------------------------------------------------------------------------"
            echo "Directory listing for $MY_WORK_DIR:"
            ls -R "$MY_WORK_DIR" || true
            echo "--------------------------------------------------------------------------------"
            exit 1
        fi
    ) &
    PIDS+=($!)
}

# Run everything except Unit Tests in parallel
run_parallel examples run_examples
run_parallel cli run_cli_tests
run_parallel dynlib run_dynlib_tests
run_parallel staticlib run_staticlib_tests
run_parallel testproject run_testproject
run_parallel wasm run_wasm_compile
run_parallel bsd_cross run_bsd_cross_compile
run_parallel http run_http_server_tests

# Wait for background tasks
exit_code=0
for p in "${PIDS[@]}"; do
    wait "$p" || exit_code=1
done

if [ $exit_code -ne 0 ]; then
    echo "::error::One or more parallel test suites failed."
    exit 1
fi

# Run unit tests last in the foreground
run_unit_tests

echo ">>> All CI Tests Passed Successfully!"
