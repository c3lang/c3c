#!/usr/bin/env bash
set -euo pipefail  # Exit on error, unset variables, and fail pipelines on any error

__wrap__() {
    # Version of C3 to install (default: latest)
    VERSION="${C3_VERSION:-latest}"
    # Installation directory (default: ~/.c3)
    C3_HOME="${C3_HOME:-$HOME/.c3}"
    # Expand '~' if present
    C3_HOME="${C3_HOME/#\~/$HOME}"
    BIN_DIR="$C3_HOME"
    # C3 compiler repository URL
    REPO="c3lang/c3c"
    REPOURL="${C3_REPOURL:-https://github.com/$REPO}"

    detect_platform() {
        # Detects the operating system
        local os_type
        os_type="$(uname -s | tr '[:upper:]' '[:lower:]')"

        case "$os_type" in
            darwin)  # macOS
                echo "macos"
                ;;
            msys*|mingw*|cygwin*)  # Windows (Git Bash / MSYS / Cygwin)
                IS_MSYS=true
                echo "windows"
                ;;
            *)
                echo $os_type
                ;;
        esac
    }

    # Determine platform string
    PLATFORM="$(detect_platform)"

    # File extension for the archive (ZIP for Windows, TAR.GZ for others)
    EXT=".tar.gz"
    BINARY="c3-${PLATFORM}"
    if [[ "${IS_MSYS:-false}" == true ]]; then
        EXT=".zip"
    fi

    # Determine the download URL (latest release or specific version)
    if [[ "$VERSION" == "latest" ]]; then
        URL="${REPOURL%/}/releases/latest/download/${BINARY}${EXT}"
    else
        URL="${REPOURL%/}/releases/download/v${VERSION#v}/${BINARY}${EXT}"
    fi

    # Temporary file for the downloaded archive
    TEMP_FILE="$(mktemp "${TMPDIR:-/tmp}/.C3_install.XXXXXXXX")"
    trap 'rm -f "$TEMP_FILE"' EXIT  # Ensure temp file is deleted on exit

    download_file() {
        # Download the archive using curl or wget
        # Check that the curl version is not 8.8.0, which is broken for --write-out
        # https://github.com/curl/curl/issues/13845
        if command -v curl >/dev/null && [[ "$(curl --version | awk 'NR==1{print $2}')" != "8.8.0" ]]; then
            curl -SL "$URL" -o "$TEMP_FILE"
        elif command -v wget >/dev/null; then
            wget -O "$TEMP_FILE" "$URL"
        else
            echo "Error: curl or wget is required." >&2
            exit 1
        fi
    }

    echo "Downloading C3 ($VERSION) from $URL..."
    download_file

    # Remove existing installation and extract the new one
    rm -rf "$BIN_DIR"
    if [[ "$EXT" == ".zip" ]]; then
        unzip "$TEMP_FILE" -d "$HOME"
    else
        tar -xzf "$TEMP_FILE" -C "$HOME"
    fi

    # Move extracted folder to installation directory
    mv "$HOME/c3" "$BIN_DIR"
    chmod +x "$BIN_DIR/c3c"  # Ensure compiler binary is executable
    echo "✅ Installation completed in $BIN_DIR"

    # Update PATH unless suppressed by environment variable
    if [ -n "${C3_NO_PATH_UPDATE:-}" ]; then
        echo "No path update because C3_NO_PATH_UPDATE is set"
    else
        update_shell() {
            FILE="$1"
            LINE="$2"

            # Create shell config file if missing
            if [ ! -f "$FILE" ]; then
                touch "$FILE"
            fi

            # Add the PATH line if not already present
            if ! grep -Fxq "$LINE" "$FILE"; then
                echo "Updating '${FILE}'"
                echo "$LINE" >>"$FILE"
                echo "Please restart or source your shell."
            fi
        }

        # Detect the current shell and add C3 to its PATH
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
}
__wrap__
