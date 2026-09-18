#!/usr/bin/env python3
import shutil
import sys
from pathlib import Path

if len(sys.argv) != 3:
    print(f"Usage: {sys.argv[0]} <src_lib_dir> <dst_lib_dir>")
    sys.exit(1)

src = Path(sys.argv[1])
dst = Path(sys.argv[2])

if src.resolve() == dst.resolve():
    sys.exit(0)

if dst.exists():
    shutil.rmtree(dst)

shutil.copytree(src, dst)