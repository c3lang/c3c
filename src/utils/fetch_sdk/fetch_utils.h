#pragma once

/**
 * Returns the platform-appropriate cache directory for a given SDK subdirectory.
 *
 * Resolution order:
 *  - Windows:     %LOCALAPPDATA%/c3/<subdir>
 *  - Linux/macOS: $XDG_CACHE_HOME/c3/<subdir>
 *                 $HOME/.cache/c3/<subdir>
 *  - Fallback:    <executable_dir>/<subdir>
 */
char *get_cache_output_path(const char *subdir);
