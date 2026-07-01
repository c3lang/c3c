#pragma once

#include <stdbool.h>
#include <stdio.h>
#include "xar.h"

bool pbzx_extract(const XarFile *file, FILE *out);
