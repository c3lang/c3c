#!/usr/bin/env bash
# Usage: ./package_build.sh <c3c_path> <output_name> <format: tar|zip>

if [ $# -lt 2 ]; then
    echo "Usage: ./package_build.sh <path_to_c3c_binary> <output_name> <format: tar|zip>"
    exit 1
fi

set -ex

C3C_BIN="$(realpath "$1")"
OUT_NAME="$2"
FORMAT="$3"
BUILD_DIR="$(dirname "$C3C_BIN")" # Assuming c3c is in build/ or bin/

# Go to repo root
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
cd "$ROOT_DIR" || exit 1

echo ">>> Packaging $OUT_NAME.$FORMAT from $C3C_BIN"

# Create temp directory
WORK_DIR=$(mktemp -d 2>/dev/null || mktemp -d -t 'c3_package')
echo ">>> Setting up packaging workspace in: $WORK_DIR"

cleanup() {
    echo ">>> Cleaning up..."
    cd "$ROOT_DIR" || cd ..
    rm -rf "$WORK_DIR"
}
trap cleanup EXIT

mkdir -p "$WORK_DIR/c3"
cd "$WORK_DIR" || exit 1

# Copy common files
cp -r "$ROOT_DIR/lib" c3/
cp "$ROOT_DIR/README.md" c3/
cp "$ROOT_DIR/releasenotes.md" c3/

# Download MANUAL.md
if command -v curl &> /dev/null; then
    curl -sSfL -o c3/MANUAL.md https://c3-lang.org/all.md || echo "Warning: failed to download MANUAL.md via curl"
elif command -v wget &> /dev/null; then
    wget -qO c3/MANUAL.md https://c3-lang.org/all.md || echo "Warning: failed to download MANUAL.md via wget"
else
    echo "Warning: Neither curl nor wget found, MANUAL.md will be missing"
fi

# Download c3fmt
C3FMT_URL=""
C3FMT_NAME=""
if [[ "$OUT_NAME" == *windows* || "$C3C_BIN" == *.exe ]]; then
    C3FMT_URL="https://github.com/lmichaudel/c3fmt/releases/latest/download/c3fmt-windows.exe"
    C3FMT_NAME="c3fmt.exe"
elif [[ "$OUT_NAME" == *linux* ]]; then
    C3FMT_URL="https://github.com/lmichaudel/c3fmt/releases/latest/download/c3fmt-linux"
    C3FMT_NAME="c3fmt"
elif [[ "$OUT_NAME" == *macos* || "$OUT_NAME" == *mac* ]]; then
    C3FMT_URL="https://github.com/lmichaudel/c3fmt/releases/latest/download/c3fmt-macos"
    C3FMT_NAME="c3fmt"
fi

if [[ -n "$C3FMT_URL" ]]; then
    echo ">>> Downloading c3fmt from $C3FMT_URL"
    if command -v curl &> /dev/null; then
        curl -sSfL -o "c3/$C3FMT_NAME" "$C3FMT_URL" || echo "Warning: failed to download c3fmt"
    elif command -v wget &> /dev/null; then
        wget -qO "c3/$C3FMT_NAME" "$C3FMT_URL" || echo "Warning: failed to download c3fmt"
    else
        echo "Warning: Neither curl nor wget found, c3fmt will be missing"
    fi
    if [[ -f "c3/$C3FMT_NAME" && "$C3FMT_NAME" == "c3fmt" ]]; then
        chmod +x "c3/$C3FMT_NAME"
    fi
fi

# Copy binaries
cp "$C3C_BIN" c3/
if [[ -f "$BUILD_DIR/c3c.pdb" ]]; then cp "$BUILD_DIR/c3c.pdb" c3/; fi

if [[ -d "$BUILD_DIR/c3c_rt" ]]; then
    echo "Found c3c_rt directory at $BUILD_DIR/c3c_rt"
    cp -r "$BUILD_DIR/c3c_rt" c3/
else
    echo "Warning: c3c_rt not found in $BUILD_DIR"
fi

# Compress
if [[ "$FORMAT" == "zip" ]]; then
    if command -v zip &> /dev/null; then
        # Standard Unix zip
        zip -r "$OUT_NAME.zip" c3
    elif command -v 7z &> /dev/null; then
        # for Windows/7-Zip fallback
        echo "Info: 'zip' command not found. Using '7z'..."
        7z a -tzip "$OUT_NAME.zip" c3
    else
        echo "Error: Neither 'zip' nor '7z' found."
        exit 1
    fi
    
    mv "$OUT_NAME.zip" "$ROOT_DIR/"
else
    tar -czf "$OUT_NAME.tar.gz" c3
    mv "$OUT_NAME.tar.gz" "$ROOT_DIR/"
fi

echo ">>> Package created: $OUT_NAME.$FORMAT"