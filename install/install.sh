#!/bin/sh
set -eu

__wrap__() {
    VERSION="${C3_VERSION:-latest}"
    C3_HOME="${C3_HOME:-$HOME/.c3}"
    case "$C3_HOME" in
    '~' | '~'/*) C3_HOME="${HOME-}${C3_HOME#\~}" ;; # expand tilde
    esac
    BIN_DIR="$C3_HOME"

    REPOURL="${C3_REPOURL:-https://github.com/c3lang/c3c}"
    PLATFORM="$(uname -s)"
    ARCH="${C3_ARCH:-$(uname -m)}"
    IS_MSYS=false

    if [ "${PLATFORM-}" = "Darwin" ]; then
        PLATFORM="macos"
    elif [ "${PLATFORM-}" = "Linux" ]; then
        OS="$(cat /etc/issue | grep -o Ubuntu)"
        if [ "${OS-}" = "Ubuntu" ]; then
            PLATFORM="ubuntu-22"
        else
            PLATFORM="debian"
        fi
    elif [ "${PLATFORM-}" = "Openbsd" ]; then
        PLATFORM="openbsd"
    elif [ "$(uname -o)" = "Msys" ]; then
        IS_MSYS=true
        PLATFORM="windows"
    fi

    # Not yet
    # case "${ARCH-}" in
    # arm64 | aarch64) ARCH="aarch64" ;;
    # esac

    # BINARY="c3-${ARCH}-${PLATFORM}"
    BINARY="c3-${PLATFORM}"
    if $IS_MSYS; then
        EXTENSION=".zip"
        hash unzip 2>/dev/null || EXTENSION=".exe"
    else
        EXTENSION=".tar.gz"
        hash tar 2>/dev/null || EXTENSION=''
    fi

    if [ "$VERSION" = "latest" ]; then
        DOWNLOAD_URL="${REPOURL%/}/releases/latest/download/${BINARY}${EXTENSION-}"
    else
        # Check if version is incorrectly specified without prefix 'v', and prepend 'v' in this case
        DOWNLOAD_URL="${REPOURL%/}/releases/download/v${VERSION#v}/${BINARY}${EXTENSION-}"
    fi

    printf "This script will automatically download and install C3 (%s) for you.\nGetting it from this url: %s\n" "$VERSION" "$DOWNLOAD_URL"

    HAVE_CURL=false
    HAVE_CURL_8_8_0=false
    if hash curl 2>/dev/null; then
        # Check that the curl version is not 8.8.0, which is broken for --write-out
        # https://github.com/curl/curl/issues/13845
        if [ "$(curl --version | (
            IFS=' ' read -r _ v _
            printf %s "${v-}"
        ))" = "8.8.0" ]; then
            HAVE_CURL_8_8_0=true
        else
            HAVE_CURL=true
        fi
    fi

    HAVE_WGET=true
    hash wget 2>/dev/null || HAVE_WGET=false

    if ! $HAVE_CURL && ! $HAVE_WGET; then
        echo "error: you need either 'curl' or 'wget' installed for this script." >&2
        if $HAVE_CURL_8_8_0; then
            echo "error: curl 8.8.0 is known to be broken, please use a different version" >&2
            if $IS_MSYS; then
                echo "A common way to get an updated version of curl is to upgrade Git for Windows:" >&2
                echo "      https://gitforwindows.org/" >&2
            fi
        fi
        exit 1
    fi

    TEMP_FILE="$(mktemp "${TMPDIR:-/tmp}/.C3_install.XXXXXXXX")"

    cleanup() {
        rm -f "$TEMP_FILE"
    }

    trap cleanup EXIT

    # Test if stdout is a terminal before showing progress
    if [ ! -t 1 ]; then
        CURL_OPTIONS="--silent" # --no-progress-meter is better, but only available in 7.67+
        WGET_OPTIONS="--no-verbose"
    else
        CURL_OPTIONS="--no-silent"
        WGET_OPTIONS="--show-progress"
    fi

    if $HAVE_CURL; then
        CURL_ERR=0
        HTTP_CODE="$(curl -SL "$DOWNLOAD_URL" --output "$TEMP_FILE" --write-out "%{http_code}")" || CURL_ERR=$?
        case "$CURL_ERR" in
        35 | 53 | 54 | 59 | 66 | 77)
            if ! $HAVE_WGET; then
                echo "error: when download '${DOWNLOAD_URL}', curl has some local ssl problems with error $CURL_ERR" >&2
                exit 1
            fi
            # fallback to wget
            ;;
        0)
            if [ "${HTTP_CODE}" -lt 200 ] || [ "${HTTP_CODE}" -gt 299 ]; then
                echo "error: '${DOWNLOAD_URL}' is not available" >&2
                exit 1
            fi
            HAVE_WGET=false # download success, skip wget
            ;;
        *)
            echo "error: when download '${DOWNLOAD_URL}', curl fails with with error $CURL_ERR" >&2
            exit 1
            ;;
        esac
    fi

    if $HAVE_WGET && ! wget $WGET_OPTIONS --output-document="$TEMP_FILE" "$DOWNLOAD_URL"; then
        echo "error: '${DOWNLOAD_URL}' is not available" >&2
        exit 1
    fi

    # Check that file was correctly created
    if [ ! -s "$TEMP_FILE" ]; then
        echo "error: temporary file ${TEMP_FILE} not correctly created." >&2
        echo "       As a workaround, you can try set TMPDIR env variable to directory with write permissions." >&2
        exit 1
    fi

    # Extract c3 from the downloaded file
    if [ "${EXTENSION-}" = ".zip" ]; then
        unzip "$TEMP_FILE" -d "$HOME"
    elif [ "${EXTENSION-}" = ".tar.gz" ]; then
        tar -xzf "$TEMP_FILE" -C "$HOME"
    fi

    # Clean previous install
    if [ -d "${BIN_DIR}" ]; then
        rm -rf "${BIN_DIR}"
    fi
    mv "$HOME/c3" "$BIN_DIR"
    chmod +x "$BIN_DIR/c3c"
    rm -rf "$TEMP_FILE"

    echo "C3 is installed into '${BIN_DIR}'"

    # shell update can be suppressed by `C3_NO_PATH_UPDATE` env var
    if [ -n "${C3_NO_PATH_UPDATE:-}" ]; then
        echo "No path update because C3_NO_PATH_UPDATE is set"
    else
        update_shell() {
            FILE="$1"
            LINE="$2"

            # Create the file if it doesn't exist
            if [ ! -f "$FILE" ]; then
                touch "$FILE"
            fi

            # Append the line if not already present
            if ! grep -Fxq "$LINE" "$FILE"; then
                echo "Updating '${FILE}'"
                echo "$LINE" >>"$FILE"
                echo "Please restart or source your shell."
            fi
        }

        case "$(basename "${SHELL-}")" in
        bash)
            # Default to bashrc as that is used in non login shells instead of the profile.
            LINE="export PATH=\"${BIN_DIR}:\$PATH\""
            update_shell ~/.bashrc "$LINE"
            ;;

        fish)
            LINE="fish_add_path ${BIN_DIR}"
            update_shell ~/.config/fish/config.fish "$LINE"
            ;;

        zsh)
            LINE="export PATH=\"${BIN_DIR}:\$PATH\""
            update_shell ~/.zshrc "$LINE"
            ;;

        tcsh)
            LINE="set path = ( ${BIN_DIR} \$path )"
            update_shell ~/.tcshrc "$LINE"
            ;;

        '')
            echo "warn: Could not detect shell type." >&2
            echo "      Please permanently add '${BIN_DIR}' to your \$PATH to enable the 'c3c' command." >&2
            ;;

        *)
            echo "warn: Could not update shell $(basename "$SHELL")" >&2
            echo "      Please permanently add '${BIN_DIR}' to your \$PATH to enable the 'c3c' command." >&2
            ;;
        esac
    fi
} && __wrap__
