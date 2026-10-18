#!/usr/bin/env python3
import re
from pathlib import Path

root = Path(__file__).resolve().parents[2]
content = (root / "src" / "version.h").read_text(encoding="utf-8")
match = re.search(r'COMPILER_VERSION\s+"([^"]+)"', content)
if match:
    print(match.group(1), end="")
else:
    print("unknown", end="")