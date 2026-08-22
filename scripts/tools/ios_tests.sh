#!/usr/bin/env bash
# Usage: ./ios_tests.sh <path_to_c3c_binary> [target_override]

# Toggle to show output of successful parallel tasks (true: show, false: hide)
SHOW_SUCCESS_LOGS="${SHOW_SUCCESS_LOGS:-true}"

if [ $# -lt 1 ]; then
    echo "Usage: ./ios_tests.sh <path_to_c3c_binary> [target_override]"
    exit 1
fi

set -e

# --- Setup Paths & Environment ---

# Resolve Script and Real Root Directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REAL_ROOT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"

C3C_BIN="$(realpath "$1")"

# ROOT_DIR points to the actual source repository
ROOT_DIR="$REAL_ROOT_DIR"

# Target flag to be passed to --target
TARGET_FLAG="$2"

echo ">>> Running iOS Target CI Tests using C3C at: $C3C_BIN"

# check if simulator UDID is captured, without that don't run the tests
# skip for physical device as it doesn't need it
DEVICE_ID="${DEVICE_ID}"
if [[ -z "$DEVICE_ID" && "$TARGET_FLAG" != "ios-aarch64" ]]; then
    echo "::error::Cannot perform tests on simulator without UDID"
    exit 1
fi

# Detect iOS target
TARGET=$([[ "$TARGET_FLAG" == "ios-aarch64" ]] && echo "Device" || echo "Simulator")
echo ">>> Detected System: iOS ($TARGET)"

# --- Create Disposable Workspace ---

# Create temp directory
WORK_DIR=$(mktemp -d 2>/dev/null || mktemp -d -t 'c3_ios_ci_tests')
echo ">>> Setting up workspace in: $WORK_DIR"

cleanup() {
    echo ">>> Cleaning up..."
    cd "$REAL_ROOT_DIR" || cd ..
    rm -rf "$WORK_DIR"
}
trap cleanup EXIT

# --- Tests ---

# Helper to run c3c with correct workspace isolation
# and the target is passed to the --target flag,
# so it becomes a native environment for both simulator and device
run_c3c() {
    "$C3C_BIN" --target "$TARGET_FLAG" --output-dir "$MY_WORK_DIR" --build-dir "$MY_WORK_DIR" --obj-out "$MY_WORK_DIR" "$@"
}

# on iOS you cannot do compile-run, 
# if done, the kernel will kill or abort the process,
# hence we simulate c3c compile-run with this helper
sim_run() {
    local source_file="$1"
    shift
    local source_name=$(basename "$source_file")
    local target_name="${source_name%.*}"
    local target_path="$MY_WORK_DIR/$target_name"
    local compile_args=()
    
    while [[ $# -gt 0 ]]; do
        if [[ "$1" == "--" ]]; then
            shift
            break
        fi
        compile_args+=("$1")
        shift
    done
    
    run_c3c compile "$source_file" "${compile_args[@]}" -o "$target_name"
    if [ -f "$target_path" ]; then
        # xcrun simctl spawn simulates the behavior of compile-run output on the simulator
        xcrun simctl spawn "$DEVICE_ID" "$target_path" "$@"
    fi
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

    # skip spawn tests on physical device
    if [[ "$TARGET_FLAG" != "ios-aarch64" ]]; then
        sim_run examples/hello_world_many.c3
        sim_run examples/time.c3
        sim_run examples/fannkuch-redux.c3
        sim_run examples/contextfree/boolerr.c3
        sim_run examples/ls.c3
        sim_run examples/args.c3 -- foo -bar "baz baz"
    fi

    run_c3c compile --no-entry --test -g --threads 1 --target macos-x64 examples/constants.c3
}

run_cli_tests() {
    local MY_WORK_DIR="$WORK_DIR/cli"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running CLI tests (init) ---"
    (
        cd "$MY_WORK_DIR"
        run_c3c init-lib mylib
        run_c3c init myproject
        (cd myproject && run_c3c benchmark myproject --suppress-run)
        rm -rf mylib.c3l myproject
    )
}

run_dynlib_tests() {
    local MY_WORK_DIR="$WORK_DIR/dynlib"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Dynamic Lib Tests ---"
    cd "$MY_WORK_DIR"
    
    run_c3c -vv dynamic-lib "$ROOT_DIR/resources/examples/dynlib-test/add.c3" -o add
    # Skip dynamic lib spawn on physical device
    if [[ "$TARGET_FLAG" != "ios-aarch64" ]]; then
        sim_run "$ROOT_DIR/resources/examples/dynlib-test/test.c3" -l "add.dylib"
    fi
}

run_staticlib_tests() {
    local MY_WORK_DIR="$WORK_DIR/staticlib"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Static Lib Tests ---"
    cd "$MY_WORK_DIR"
    
    run_c3c -vv static-lib "$ROOT_DIR/resources/examples/staticlib-test/add.c3" -o libadd
    # Skip static lib spawn on physical device
    if [[ "$TARGET_FLAG" != "ios-aarch64" ]]; then
        sim_run "$ROOT_DIR/resources/examples/staticlib-test/test.c3" -L . -l add
    fi
}

run_testproject() {
    local MY_WORK_DIR="$WORK_DIR/testproject"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Test Project ---"
    cd "$ROOT_DIR/resources/testproject"
    
    run_c3c build -vv --trust=full --linker=builtin
    run_c3c clean
}

run_wasm_compile() {
    local MY_WORK_DIR="$WORK_DIR/wasm"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running WASM Compile Check ---"
    cd "$ROOT_DIR/resources/testfragments"
    run_c3c compile --target wasm32 -g0 --no-entry -Os wasm4.c3
}

run_http_server_tests() {
    local MY_WORK_DIR="$WORK_DIR/http"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running HTTP Server Integration Tests ---"
    if [[ "$TARGET_FLAG" == "ios-aarch64" ]]; then
        echo "::warning::Running http tests on device is not allowed. Skipping..."
        return
    fi
    
    cd "$ROOT_DIR/resources/examples"
    run_c3c compile -O1 http_server.c3 -o http_server

    OUTPUT_BIN="$MY_WORK_DIR/http_server"

    PORT=$(( 8085 + $RANDOM % 10000 ))
    echo "Starting server on port $PORT..."
    
    xcrun simctl spawn "$DEVICE_ID" "$OUTPUT_BIN" -p $PORT -r "$ROOT_DIR/resources/examples" &
    SERVER_PID=$!
    
    sleep 2

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

    echo "--- Running Unit Test Suites ---"
    if [[ "$TARGET_FLAG" == "ios-aarch64" ]]; then
        echo "::warning::Running unit tests on device is not allowed. Skipping..."
        return
    fi
    
    cd "$ROOT_DIR/test"
    run_c3c compile-test unit -O1 --suppress-run -o "unit_test"
    if [ -f "$MY_WORK_DIR/unit_test" ]; then
        xcrun simctl spawn "$DEVICE_ID" "$MY_WORK_DIR/unit_test"
    fi

    echo "--- Running Test Suite Runner ---"
    (
        cd "$MY_WORK_DIR"
        sim_run "$ROOT_DIR/test/src/test_suite_runner.c3" -O1 -- "$C3C_BIN" "$ROOT_DIR/test/test_suite/" --no-terminal
    )
}

# --- Execution ---

# Function to run a suite and capture its output in the background
PIDS=()
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
run_parallel http run_http_server_tests

# Wait for background tasks
exit_code=0
for p in "${PIDS[@]}"; do
    wait "$p" || exit_code=1
done

if [ $exit_code -ne 0 ]; then
    echo "::error::One or more parallel iOS test suites failed."
    exit 1
fi

# Run unit tests last in the foreground
run_unit_tests

echo ">>> All iOS CI Tests Passed Successfully!"
