#!/usr/bin/env bash
# Run c3c CI tests against a fully static binary across major Linux distros.
# Usage: ./scripts/tools/test_distros.sh [path/to/c3c] [distro1 distro2 ...]
# If no distros are given, all are tested.
# Requires: podman (or docker - set RUNTIME=docker)
#
# Mounts:
#   /usr/local/bin/c3c  - the static binary
#   /repo               - the repo (read-only, for tests/resources)
#   C3C_LIB             - points to lib/ so c3c finds its stdlib
#
# Only a C compiler (gcc) is needed in the container for the linker step.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"
C3C_BIN="${1:-$REPO_ROOT/build_static/c3c}"
RUNTIME="${RUNTIME:-$(command -v podman 2>/dev/null || echo docker)}"
REBUILD="${REBUILD:-0}"  # Set REBUILD=1 to force re-build of cached images

if [[ ! -f "$C3C_BIN" ]]; then
	echo "error: c3c binary not found at '$C3C_BIN'"
	echo "usage: $0 [path/to/c3c] [distro...]"
	exit 1
fi

C3C_BIN="$(realpath "$C3C_BIN")"

# --- Distro definitions ---
# Format: "image|install_command"
declare -A DISTRO_IMAGES=(
	[ubuntu]="ubuntu:24.04|apt-get update -qq && apt-get install -y -qq gcc bash"
	[debian]="debian:bookworm-slim|apt-get update -qq && apt-get install -y -qq gcc bash"
	[fedora]="fedora:latest|dnf install -y -q gcc bash"
	[alpine]="alpine:3.23|apk add --no-cache gcc musl-dev bash"
	[arch]="archlinux:latest|pacman -Sy --noconfirm gcc bash"
	[opensuse]="opensuse/tumbleweed:latest|zypper install -y gcc glibc-devel bash"
	[rocky]="rockylinux:9|dnf install -y -q gcc bash"
	[void]="ghcr.io/void-linux/void-linux:latest-full-x86_64|xbps-install -Suy xbps && xbps-install -Sy gcc bash"
)

# Resolve which distros to test
if [[ $# -gt 1 ]]; then
	shift
	DISTROS=("$@")
else
	DISTROS=("${!DISTRO_IMAGES[@]}")
fi

# Results tracking
PASSED=()
FAILED=()
SKIPPED=()
LOG_FILES=()

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m'

print_summary() {
	echo ""
	echo -e "${BOLD}  Summary${NC}"
	for d in "${PASSED[@]+"${PASSED[@]}"}";  do echo -e "  ${GREEN}✓ $d${NC}"; done
	for d in "${FAILED[@]+"${FAILED[@]}"}";  do echo -e "  ${RED}✗ $d${NC}"; done
	for d in "${SKIPPED[@]+"${SKIPPED[@]}"}"; do echo -e "  ${YELLOW}? $d (unknown)${NC}"; done
	echo ""
	# Clean up temp log files
	for f in "${LOG_FILES[@]+"${LOG_FILES[@]}"}"; do [[ -f "$f" ]] && rm -f "$f"; done
}

on_interrupt() {
	echo -e "\n${YELLOW}  Interrupted.${NC}"
	print_summary
	exit 130
}

trap print_summary EXIT
trap on_interrupt INT TERM

run_on_distro() {
	local name="$1"
	local image="${DISTRO_IMAGES[$name]%%|*}"
	local install="${DISTRO_IMAGES[$name]#*|}"
	local cached_image="localhost/c3c-distro-test:$name"

	echo ""
	echo -e "${BOLD}  Testing on: $name ($image)${NC}"

	# Build a cached image with dependencies pre-installed if not present
	if [[ "$REBUILD" == "1" ]] || ! $RUNTIME image exists "$cached_image" 2>/dev/null; then
		echo "  → Building cached image for $name..."
		if ! printf "FROM %s\nRUN %s\n" "$image" "$install" | \
				$RUNTIME build -t "$cached_image" -f - . -q; then
			echo -e "${RED}  ✗ FAILED (could not build image for $name)${NC}"
			FAILED+=("$name")
			return
		fi
	else
		echo "  → Using cached image $cached_image"
	fi

	local log_file
	log_file=$(mktemp /tmp/c3c_test_${name}_XXXXXX.log)
	LOG_FILES+=("$log_file")

	# Run against the cached image - no install step needed
	if $RUNTIME run --rm \
		-v "$REPO_ROOT:/repo:ro" \
		-v "$C3C_BIN:/usr/local/bin/c3c:ro" \
		-v "$REPO_ROOT/lib:/c3lib:ro" \
		-e "C3C_LIB=/c3lib" \
		--network host \
		"$cached_image" \
		bash -c "
			set -e
			echo '>>> Running tests on $name'
			/repo/scripts/tools/ci_tests.sh /usr/local/bin/c3c linux
		" > "$log_file" 2>&1; then
		echo -e "${GREEN}  ✓ PASSED${NC}"
		PASSED+=("$name")
	else
		echo -e "${RED}  ✗ FAILED (log: $log_file)${NC}"
		echo "--- Last 30 lines of output ---"
		tail -30 "$log_file"
		echo "--- End of output ---"
		FAILED+=("$name")
	fi
}

# --- Main ---
echo -e "${BOLD}c3c distro test suite${NC}"
echo "Binary:  $C3C_BIN"
echo "Runtime: $RUNTIME"
echo "Distros: ${DISTROS[*]}"

for distro in "${DISTROS[@]}"; do
	if [[ -z "${DISTRO_IMAGES[$distro]+x}" ]]; then
		echo -e "${YELLOW}  ? SKIP: unknown distro '$distro'${NC}"
		SKIPPED+=("$distro")
		continue
	fi
	run_on_distro "$distro"
done
# exit code: 0 if all passed, 1 if any failed (summary printed by EXIT trap)
[[ ${#FAILED[@]} -eq 0 ]]
