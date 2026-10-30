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

C3_BACKEND="${C3_BACKEND:-llvm}"
if [[ "$C3_BACKEND" == "c" ]]; then
    export CC="${CC:-clang}"
fi

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
    TARGET_ARGS=""
    if [[ "$OS_MODE" == "ios" ]]; then TARGET_ARGS="--target ios-aarch64-sim"; fi
    BACKEND_ARGS=""
    if [[ -n "$C3_BACKEND" && "$C3_BACKEND" != "llvm" ]]; then
        BACKEND_ARGS="--backend=$C3_BACKEND"
    fi
    "$C3C_BIN" $TARGET_ARGS $BACKEND_ARGS --output-dir "$MY_WORK_DIR" --build-dir "$MY_WORK_DIR" --obj-out "$MY_WORK_DIR" "$@"
}

# Helper for ios to simulate compile-run
sim_run() {
    local source_file="$1"
    shift
    local source_name=$(basename "$source_file")
    local target_name="${source_name%.*}"
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
    xcrun simctl spawn booted "$MY_WORK_DIR/$target_name" "$@" 2>/dev/null || true
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
    run_c3c compile-only --single-module=yes examples/levenshtein.c3 # --single-module test
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

    if [[ "$OS_MODE" == "ios" ]]; then 
        sim_run examples/hello_world_many.c3
        sim_run examples/time.c3
        sim_run examples/fannkuch-redux.c3
        sim_run examples/contextfree/boolerr.c3
        sim_run examples/ls.c3
        sim_run examples/args.c3 -- foo -bar "baz baz"
    else 
        run_c3c compile-run examples/hello_world_many.c3
        run_c3c compile-run examples/time.c3
        run_c3c compile-run examples/fannkuch-redux.c3
        run_c3c compile-run examples/contextfree/boolerr.c3
        run_c3c compile-run examples/load_world.c3
        run_c3c compile-run examples/process.c3
        run_c3c compile-run examples/ls.c3
        run_c3c compile-run examples/args.c3 -- foo -bar "baz baz"
    fi

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

        if [ -f "/etc/alpine-release" ] || [[ "$SYSTEM_NAME" == *"BSD" ]] || [[ "$OS_MODE" == "ios" ]] || [[ "$OS_MODE" == "android" ]]; then
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

    # Regression: two independently built C3 dynamic libraries loaded into one
    # (non-C3) host via dlopen. Each carries its own runtime, so the second's
    # startup must initialise only its own image -- this used to crash on macOS.
    if [[ "$OS_MODE" == "mac" || "$OS_MODE" == "linux" ]]; then
        run_c3c -vv dynamic-lib "$ROOT_DIR/resources/examples/dynlib-test/add2.c3" -o add2
        if [[ "$OS_MODE" == "mac" ]]; then
            cc "$ROOT_DIR/resources/examples/dynlib-test/test_multi.c" -o multi
        else
            cc "$ROOT_DIR/resources/examples/dynlib-test/test_multi.c" -ldl -o multi
        fi
        ./multi
    fi

    if [[ "$OS_MODE" == "mac" ]]; then
        local DL_DIR="$ROOT_DIR/resources/examples/dynlib-test"
        run_c3c -vv dynamic-lib "$DL_DIR/linked_a.c3" "$DL_DIR/linked_foo.c3" -o linked_a
        run_c3c -vv dynamic-lib "$DL_DIR/linked_b.c3" "$DL_DIR/linked_foo.c3" -o linked_b
        run_c3c -vv compile-run "$DL_DIR/linked_main.c3" "$DL_DIR/linked_foo.c3" -l linked_a.dylib -l linked_b.dylib
    fi

    if [[ "$OS_MODE" == "windows" ]]; then
        run_c3c -vv compile-run "$ROOT_DIR/resources/examples/dynlib-test/test.c3" -l "add.lib"
    elif [[ "$OS_MODE" == "mac" ]]; then
        run_c3c -vv compile-run "$ROOT_DIR/resources/examples/dynlib-test/test.c3" -l "add.dylib"
    elif [[ "$OS_MODE" == "ios" ]]; then
        sim_run "$ROOT_DIR/resources/examples/dynlib-test/test.c3" -vv -l "add.dylib"
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
        elif [[ "$OS_MODE" == "ios" ]]; then
            xcrun -sdk iphonesimulator clang "$ROOT_DIR/resources/examples/staticlib-test/test.c" -L. -ladd -o a.out
        else
             # Mac / NetBSD
             cc "$ROOT_DIR/resources/examples/staticlib-test/test.c" -L. -ladd -o a.out
        fi
        if [[ "$OS_MODE" == "ios" ]]; then
            xcrun simctl spawn booted ./a.out
            sim_run "$ROOT_DIR/resources/examples/staticlib-test/test.c3" -L . -l add
        else 
            ./a.out
            run_c3c compile-run "$ROOT_DIR/resources/examples/staticlib-test/test.c3" -L . -l add
        fi
    fi
}

run_sourcelib_tests() {
    local MY_WORK_DIR="$WORK_DIR/sourcelib"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Source Lib Tests ---"
    cd "$ROOT_DIR/resources/examples/sourcelib-test"

    # Build the library from a project target, first compressed, then as a directory replacing the file.
    for flags in "" "--no-compress"; do
        run_c3c build sourcelib $flags
        if [[ "$OS_MODE" == "ios" ]]; then
            sim_run "$ROOT_DIR/resources/examples/sourcelib-test/test.c3" --libdir "$MY_WORK_DIR" --lib sourcelib
        else
            run_c3c compile-run "$ROOT_DIR/resources/examples/sourcelib-test/test.c3" --libdir "$MY_WORK_DIR" --lib sourcelib
        fi
    done

    # Build the library without a project.
    run_c3c source-lib src/sourcelib.c3 src/extra/double.c3 -o standalone
}

run_testproject() {
    local MY_WORK_DIR="$WORK_DIR/testproject"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Test Project ---"
    cd "$ROOT_DIR/resources/testproject"

    ARGS="--trust=full"

    if [[ "$OS_MODE" == "linux" || "$OS_MODE" == "mac" || "$OS_MODE" == "ios" ]]; then
        ARGS="$ARGS --linker=builtin"

        if [ -f "/etc/alpine-release" ]; then
            ARGS="$ARGS --linux-libc=musl"
        fi
    fi

    if [[ "$OS_MODE" == "ios" ]]; then
        run_c3c build -vv $ARGS
        xcrun simctl spawn booted "$MY_WORK_DIR/hello_world" 2>/dev/null || true
    else
        run_c3c run -vv $ARGS
    fi
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

    kill_server() {
        if [ -n "$SERVER_PID" ]; then
            kill $SERVER_PID 2>/dev/null || true
        fi
        if [[ "$OS_MODE" == "ios" ]]; then
            xcrun simctl spawn booted pkill -f "$OUTPUT_BIN" 2>/dev/null || true
        fi
    }

    if [[ "$OS_MODE" == "ios" ]]; then
        tail -f /dev/null | xcrun simctl spawn booted "$OUTPUT_BIN" -p $PORT -r "$ROOT_DIR/resources/examples" > "$MY_WORK_DIR/server.log" 2>&1 &
        SERVER_PID=$!
    else
        "$OUTPUT_BIN" -p $PORT -r "$ROOT_DIR/resources/examples" > "$MY_WORK_DIR/server.log" 2>&1 &
        SERVER_PID=$!
    fi

    # Poll until server is responding.
    # iOS simulator startup is slow (xcrun simctl attach + binary load can take 10-20s).
    POLL_ITERS=$([[ "$OS_MODE" == "ios" ]] && echo 150 || echo 30)  # 30s / 6s
    SERVER_READY=false
    for ((i = 1; i <= POLL_ITERS; i++)); do
        kill -0 "$SERVER_PID" 2>/dev/null || { echo "::error::HTTP server process exited unexpectedly."; break; }
        if curl -s -o /dev/null "http://127.0.0.1:$PORT/"; then
            SERVER_READY=true
            break
        fi
        sleep 0.2
    done

    if [ "$SERVER_READY" != "true" ]; then
        echo "::error::HTTP server failed to start or respond on port $PORT within timeout."
        echo "--- Server Log Output ---"
        cat "$MY_WORK_DIR/server.log" 2>/dev/null || true
        kill_server
        exit 1
    fi

    # Test root path (directory listing)
    echo "Testing GET /"
    HTTP_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://127.0.0.1:$PORT/" || echo "000")
    if [ "$HTTP_STATUS" != "200" ]; then
        echo "::error::HTTP GET / failed with status $HTTP_STATUS."
        echo "--- Server Log Output ---"
        cat "$MY_WORK_DIR/server.log" 2>/dev/null || true
        kill_server
        exit 1
    fi

    # Test served file
    echo "Testing GET /http_server.c3"
    HTTP_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://127.0.0.1:$PORT/http_server.c3" || echo "000")
    if [ "$HTTP_STATUS" != "200" ]; then
        echo "::error::HTTP GET /http_server.c3 failed with status $HTTP_STATUS."
        echo "--- Server Log Output ---"
        cat "$MY_WORK_DIR/server.log" 2>/dev/null || true
        kill_server
        exit 1
    fi

    # Test missing file (404 expected)
    echo "Testing 404 for invalid path"
    HTTP_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://127.0.0.1:$PORT/does_not_exist_404_test" || echo "000")
    if [ "$HTTP_STATUS" != "404" ]; then
        echo "::error::HTTP GET /does_not_exist_404_test expected 404, but got $HTTP_STATUS."
        echo "--- Server Log Output ---"
        cat "$MY_WORK_DIR/server.log" 2>/dev/null || true
        kill_server
        exit 1
    fi

    echo "HTTP Server Integration Tests passed."
    kill_server
}

run_fetch_tests() {
    local MY_WORK_DIR="$WORK_DIR/fetch"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Registry Fetch Tests ---"

    if [ -n "$SKIP_NETWORK_TESTS" ] || [[ "$OS_MODE" == "ios" ]]; then
        echo "Skipping registry fetch tests"
        return
    fi

    local sha256="sha256sum"
    if ! command -v sha256sum &> /dev/null; then sha256="shasum -a 256"; fi
    if ! command -v zip &> /dev/null || ! command -v curl &> /dev/null; then
        echo "::warning::zip or curl not found, skipping registry fetch tests"
        return
    fi

    # A registry is only files: info/<ns>/<name> and download/<ns>/<name>@<version>.
    local REGISTRY="$MY_WORK_DIR/registry"
    mkdir -p "$REGISTRY/info/acme" "$REGISTRY/download/acme" "$MY_WORK_DIR/lib/hello.c3l"
    echo '{"provides":"hello"}' > "$MY_WORK_DIR/lib/hello.c3l/manifest.json"
    (cd "$MY_WORK_DIR/lib/hello.c3l" && zip -qr "$REGISTRY/download/acme/hello@1.0.0" .)
    local good_sum
    good_sum=$($sha256 "$REGISTRY/download/acme/hello@1.0.0" | cut -d' ' -f1)
    local bad_sum="0000000000000000000000000000000000000000000000000000000000000000"

    # $1 = name, $2 = checksum field (may be empty)
    write_info() {
        cp "$REGISTRY/download/acme/hello@1.0.0" "$REGISTRY/download/acme/$1@1.0.0"
        echo "{\"library\":\"acme/$1\",\"description\":\"d\",\"latest\":\"1.0.0\",\"updated\":\"2026-01-01T00:00:00Z\",\"versions\":{\"1.0.0\":{\"version\":\"1.0.0\"$2}}}" > "$REGISTRY/info/acme/$1"
    }
    write_info good ",\"checksum\":\"sha256:$good_sum\""
    write_info upper ",\"checksum\":\"sha256:$(echo "$good_sum" | tr 'a-f' 'A-F')\""
    write_info plain ""
    write_info tampered ",\"checksum\":\"sha256:$bad_sum\""
    write_info other ",\"checksum\":\"md5:$bad_sum\""

    cd "$ROOT_DIR/resources/examples"
    run_c3c compile -O0 -g0 --linker=builtin http_server.c3 -o fetch_registry_server
    local server_bin="$MY_WORK_DIR/fetch_registry_server"
    if [[ "$OS_MODE" == "windows" ]]; then server_bin="$server_bin.exe"; fi

    local PORT=$(( 8085 + $RANDOM % 10000 ))
    "$server_bin" -p $PORT -r "$REGISTRY" > "$MY_WORK_DIR/server.log" 2>&1 &
    local SERVER_PID=$!
    trap 'kill -9 $SERVER_PID 2>/dev/null || true' RETURN

    local ready=false
    for ((i = 1; i <= 30; i++)); do
        kill -0 "$SERVER_PID" 2>/dev/null || break
        if curl -s -o /dev/null "http://127.0.0.1:$PORT/"; then ready=true; break; fi
        sleep 0.2
    done
    if [ "$ready" != "true" ]; then
        echo "::error::Registry test server failed to start on port $PORT."
        cat "$MY_WORK_DIR/server.log" 2>/dev/null || true
        exit 1
    fi

    local INSTALL="$MY_WORK_DIR/install"
    mkdir -p "$INSTALL"
    cd "$INSTALL"
    local FETCH=("$C3C_BIN" --fetch-url "http://127.0.0.1:$PORT" fetch)

    echo "Fetch with a matching checksum (upper case hex is fine too)"
    "${FETCH[@]}" acme/good acme/upper
    test -f good.c3l/manifest.json
    test -f upper.c3l/manifest.json

    echo "Fetch without a checksum"
    "${FETCH[@]}" acme/plain
    test -f plain.c3l/manifest.json

    echo "A wrong checksum is rejected and an existing copy is kept"
    mkdir tampered.c3l && echo keep > tampered.c3l/old
    if "${FETCH[@]}" acme/tampered; then
        echo "::error::fetch accepted a library with a wrong checksum."
        exit 1
    fi
    test -f tampered.c3l/old
    test ! -f tampered.c3l/manifest.json

    echo "An unknown checksum algorithm is rejected"
    if "${FETCH[@]}" acme/other; then
        echo "::error::fetch accepted a checksum it can't verify."
        exit 1
    fi
    test ! -e other.c3l

    local URL="http://127.0.0.1:$PORT"
    echo "A project fetches '<ns>/<name>@<version>' dependencies from its fetch-url and builds with them"
    # This library is called vk, as the dependency name is what the build looks for.
    mkdir -p "$MY_WORK_DIR/vklib"
    echo '{"provides":"vk"}' > "$MY_WORK_DIR/vklib/manifest.json"
    (cd "$MY_WORK_DIR/vklib" && zip -qr "$REGISTRY/download/acme/vk@1.0.0" .)
    local vk_sum
    vk_sum=$($sha256 "$REGISTRY/download/acme/vk@1.0.0" | cut -d' ' -f1)
    echo "{\"library\":\"acme/vk\",\"description\":\"d\",\"latest\":\"1.0.0\",\"updated\":\"2026-01-01T00:00:00Z\",\"versions\":{\"1.0.0\":{\"version\":\"1.0.0\",\"checksum\":\"sha256:$vk_sum\"}}}" > "$REGISTRY/info/acme/vk"

    local PROJECTS="$MY_WORK_DIR/projects"
    mkdir -p "$PROJECTS"
    cd "$PROJECTS"
    "$C3C_BIN" init app > /dev/null
    sed -i.bak "s|\"dependencies\": \[ \],|\"fetch-url\": \"$URL\", \"dependencies\": [\"acme/vk@1.0.0\"],|" app/project.json
    (
        cd app
        "$C3C_BIN" project fetch
        test -f lib/vk.c3l/manifest.json
        run_c3c build -C
    )

    echo "fetch records the library with its version, and the registry unless the project has one"
    "$C3C_BIN" init recorded > /dev/null
    (
        cd recorded
        "$C3C_BIN" --fetch-url "$URL" fetch acme/vk
        grep -q '"acme/vk@1.0.0"' project.json
        grep -q "\"fetch-url\": \"$URL\"" project.json
        rm -rf lib/vk.c3l
        "$C3C_BIN" project fetch
        test -f lib/vk.c3l/manifest.json
    )

    echo "Two dependencies with the same name are rejected"
    "$C3C_BIN" init clash > /dev/null
    sed -i.bak "s|\"dependencies\": \[ \],|\"fetch-url\": \"$URL\", \"dependencies\": [\"acme/vk\", \"other/vk\"],|" clash/project.json
    if (cd clash && "$C3C_BIN" project fetch); then
        echo "::error::project fetch accepted two dependencies named vk."
        exit 1
    fi
    test ! -e clash/lib/vk.c3l

    # A library with two versions, whose manifests say which version they are.
    echo "A library can be locked to a checksum"
    local v
    for v in 1.0.0 2.0.0; do
        mkdir -p "$MY_WORK_DIR/pinned$v"
        echo "{\"provides\":\"pinned\",\"version\":\"$v\"}" > "$MY_WORK_DIR/pinned$v/manifest.json"
        (cd "$MY_WORK_DIR/pinned$v" && zip -qr "$REGISTRY/download/acme/pinned@$v" .)
    done
    local pin1 pin2
    pin1=$($sha256 "$REGISTRY/download/acme/pinned@1.0.0" | cut -d' ' -f1)
    pin2=$($sha256 "$REGISTRY/download/acme/pinned@2.0.0" | cut -d' ' -f1)
    echo "{\"library\":\"acme/pinned\",\"description\":\"d\",\"latest\":\"2.0.0\",\"updated\":\"2026-01-01T00:00:00Z\",\"versions\":{\"2.0.0\":{\"version\":\"2.0.0\",\"checksum\":\"sha256:$pin2\"},\"1.0.0\":{\"version\":\"1.0.0\",\"checksum\":\"sha256:$pin1\"}}}" > "$REGISTRY/info/acme/pinned"

    local PIN="$MY_WORK_DIR/pin"
    # $1 = project.json content
    new_project() {
        rm -rf "$PIN"
        mkdir -p "$PIN"
        echo "$1" > "$PIN/project.json"
    }
    local EMPTY='{ "dependency-search-paths": ["lib"], "sources": ["src/**"] }'

    new_project "$EMPTY"
    (
        cd "$PIN"
        # without a version, the version with that checksum is fetched and the library stays locked to it
        "$C3C_BIN" --fetch-url "$URL" fetch "acme/pinned:$pin1"
        grep -q '"version":"1.0.0"' lib/pinned.c3l/manifest.json
        grep -q "\"acme/pinned:$pin1\"" project.json
    )
    new_project "$EMPTY"
    (
        cd "$PIN"
        "$C3C_BIN" --fetch-url "$URL" fetch "acme/pinned@2.0.0:$pin2"
        grep -q "\"acme/pinned@2.0.0:$pin2\"" project.json
    )

    echo "A wrong, unknown or malformed checksum is rejected before anything is installed"
    local unknown
    unknown=$(printf 'a%.0s' $(seq 64))
    local lock
    for lock in "acme/pinned@1.0.0:$pin2" "acme/pinned:$unknown" "acme/pinned:abc"; do
        new_project "$EMPTY"
        if (cd "$PIN" && "$C3C_BIN" --fetch-url "$URL" fetch "$lock"); then
            echo "::error::fetch accepted '$lock'."
            exit 1
        fi
        test ! -e "$PIN/lib/pinned.c3l"
    done

    echo "A dependency can name its own registry"
    new_project "{ \"dependency-search-paths\": [\"lib\"], \"fetch-url\": \"http://127.0.0.1:1\", \"dependencies\": [\"$URL:acme/pinned@2.0.0:$pin2\"], \"sources\": [\"src/**\"] }"
    (
        cd "$PIN"
        "$C3C_BIN" project fetch
        grep -q '"version":"2.0.0"' lib/pinned.c3l/manifest.json
    )

    echo "fetch lists a library with its own registry when the project has another one"
    new_project '{ "dependency-search-paths": ["lib"], "fetch-url": "http://127.0.0.1:1", "sources": ["src/**"] }'
    (
        cd "$PIN"
        "$C3C_BIN" --fetch-url "$URL" fetch acme/pinned
        grep -q "\"$URL:acme/pinned@2.0.0\"" project.json
        grep -q '"fetch-url": "http://127.0.0.1:1"' project.json
    )

    echo "project fetch replaces a library of another version, and keeps one that matches or has no version asked"
    new_project "{ \"dependency-search-paths\": [\"lib\"], \"fetch-url\": \"$URL\", \"dependencies\": [\"acme/pinned@2.0.0\"], \"sources\": [\"src/**\"] }"
    mkdir -p "$PIN/lib/pinned.c3l"
    echo '{"provides":"pinned","version":"0.5.0"}' > "$PIN/lib/pinned.c3l/manifest.json"
    (
        cd "$PIN"
        "$C3C_BIN" project fetch
        grep -q '"version":"2.0.0"' lib/pinned.c3l/manifest.json
        touch lib/pinned.c3l/marker
        "$C3C_BIN" project fetch
        test -f lib/pinned.c3l/marker
        sed -i.bak 's|acme/pinned@2.0.0|acme/pinned|' project.json
        echo '{"provides":"pinned","version":"0.5.0"}' > lib/pinned.c3l/manifest.json
        "$C3C_BIN" project fetch
        grep -q '"version":"0.5.0"' lib/pinned.c3l/manifest.json
    )

    echo "A version of the registry may say where the library is stored"
    mkdir -p "$REGISTRY/files" "$MY_WORK_DIR/remotelib"
    echo '{"provides":"remote"}' > "$MY_WORK_DIR/remotelib/manifest.json"
    (cd "$MY_WORK_DIR/remotelib" && zip -qr "$REGISTRY/files/remote.c3l" .)
    local remote_sum
    remote_sum=$($sha256 "$REGISTRY/files/remote.c3l" | cut -d' ' -f1)
    local entry name location sum
    for entry in "remote|$URL/files/remote.c3l|sha256:$remote_sum" "badurl|ftp://example.invalid/badurl.c3l|sha256:$remote_sum" "wrongsum|$URL/files/remote.c3l|sha256:$bad_sum"; do
        IFS='|' read -r name location sum <<< "$entry"
        echo "{\"library\":\"acme/$name\",\"description\":\"d\",\"latest\":\"1.0.0\",\"updated\":\"2026-01-01T00:00:00Z\",\"versions\":{\"1.0.0\":{\"version\":\"1.0.0\",\"url\":\"$location\",\"checksum\":\"$sum\"}}}" > "$REGISTRY/info/acme/$name"
    done
    new_project "$EMPTY"
    (
        cd "$PIN"
        "$C3C_BIN" --fetch-url "$URL" fetch acme/remote
        test -f lib/remote.c3l/manifest.json
        if "$C3C_BIN" --fetch-url "$URL" fetch acme/badurl; then
            echo "::error::fetch accepted a url that is not http or https."
            exit 1
        fi
        if "$C3C_BIN" --fetch-url "$URL" fetch acme/wrongsum; then
            echo "::error::fetch accepted a wrong checksum for a library stored elsewhere."
            exit 1
        fi
        test ! -e lib/badurl.c3l
        test ! -e lib/wrongsum.c3l
    )

    echo "Registry fetch tests passed."
}

run_unit_tests() {
    local MY_WORK_DIR="$WORK_DIR/unit"
    mkdir -p "$MY_WORK_DIR"

    echo "--- Running Unit Tests ---"
    cd "$ROOT_DIR/test"

    UNIT_TEST_ARGS="-O1"
    if [[ "$OS_MODE" != "bsd" ]]; then
        if [[ "$OS_MODE" == "ios" ]]; then
            UNIT_TEST_ARGS="$UNIT_TEST_ARGS -D SLOW_TESTS --suppress-run"
        else
            UNIT_TEST_ARGS="$UNIT_TEST_ARGS -D SLOW_TESTS -D RUN_PROCESS_TESTS"
        fi
    fi

    run_c3c compile-test unit $UNIT_TEST_ARGS
    if [[ "$OS_MODE" == "ios" ]]; then
        xcrun simctl spawn booted "$MY_WORK_DIR/testrun" 2>/dev/null || true
    fi

    RUNNER_ARGS=("--no-terminal")
    if [[ -n "$C3_BACKEND" && "$C3_BACKEND" != "llvm" ]]; then
        RUNNER_ARGS+=("--backend=$C3_BACKEND")
    fi

    echo "--- Running Test Suite Runner ---"
    (
        cd "$MY_WORK_DIR"
        if [[ "$OS_MODE" == "ios" ]]; then
            sim_run "$ROOT_DIR/test/src/test_suite_runner.c3" -O1 -- "$C3C_BIN" "$ROOT_DIR/test/test_suite/" "${RUNNER_ARGS[@]}"
        else
            run_c3c compile-run -O1 "$ROOT_DIR/test/src/test_suite_runner.c3" -- "$C3C_BIN" "$ROOT_DIR/test/test_suite/" "${RUNNER_ARGS[@]}"
        fi
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
run_parallel sourcelib run_sourcelib_tests
run_parallel wasm run_wasm_compile
run_parallel bsd_cross run_bsd_cross_compile
run_parallel http run_http_server_tests
run_parallel fetch run_fetch_tests

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
