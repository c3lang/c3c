#pragma once

#include <stdbool.h>
#include "cpio.h"
#include "xar.h"

bool pbzx_extract(const XarFile *file, Cpio *cpio);
