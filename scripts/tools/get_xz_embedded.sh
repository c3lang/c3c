#!/bin/sh

SPDX_LI='SPDX''-License-Identifier'': 0BSD'

CWD=$(pwd)
XZ_DEP="$CWD/dependencies/xz"
OUT="$XZ_DEP/standalone.c"

add() {
    "$CWD/scripts/tools/strip_comments.awk" "$1" | grep -v -e "$SPDX_LI" -e '#.*include "' >> "$OUT"
}

cd /tmp || exit

git clone --depth 1 "https://github.com/tukaani-project/xz-embedded.git" "xz_embedded"
cd xz_embedded || exit

cat "linux/include/linux/xz.h" > "$XZ_DEP/xz.h"
cat > "$OUT" <<EOF
/* SPDX-License-Identifier: 0BSD */

#include <stdbool.h>
#include <string.h>

#include "xz.h"
#include "xz_config.h"

EOF

add "userspace/xz_config.h"
add "linux/lib/xz/xz_private.h"
add "linux/lib/xz/xz_stream.h"
add "linux/lib/xz/xz_stream.h"
add "linux/lib/xz/xz_lzma2.h"
add "linux/lib/xz/xz_crc32.c"
add "linux/lib/xz/xz_crc64.c"
add "linux/lib/xz/xz_sha256.c"
add "linux/lib/xz/xz_dec_stream.c"
add "linux/lib/xz/xz_dec_lzma2.c"
add "linux/lib/xz/xz_dec_bcj.c"

cd ..
rm -rf xz_embedded