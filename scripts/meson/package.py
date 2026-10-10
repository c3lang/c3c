#!/usr/bin/env python3
import argparse
import os
import shutil
import stat
import sys
import tarfile
import urllib.request
import zipfile
from pathlib import Path


def download(url, dest):
    try:
        req = urllib.request.Request(
            url, headers={"User-Agent": "c3c-packager"}
        )
        with urllib.request.urlopen(req) as resp, open(dest, "wb") as out:
            shutil.copyfileobj(resp, out)
    except Exception as e:
        sys.stderr.write(f"Warning: Failed to download {url}: {e}\n")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--bin", required=True)
    parser.add_argument("--out", default="c3c")
    parser.add_argument("--format", choices=["zip", "tar"], default="zip")
    args = parser.parse_args()

    c3c_bin = Path(args.bin).resolve()
    build_dir = c3c_bin.parent
    root_dir = Path(__file__).resolve().parent.parent.parent

    stage = build_dir / "c3_package" / "c3"
    if stage.parent.exists():
        shutil.rmtree(stage.parent)
    stage.mkdir(parents=True)

    # 1. Standard library and docs
    shutil.copytree(root_dir / "lib", stage / "lib")
    for doc in ("README.md", "releasenotes.md"):
        src = root_dir / doc
        if src.exists():
            shutil.copy2(src, stage / doc)

    download("https://c3-lang.org/all.md", stage / "MANUAL.md")

    # 2. c3fmt binary
    is_win = sys.platform == "win32" or c3c_bin.suffix == ".exe"
    is_mac = sys.platform == "darwin"
    fmt_name = "c3fmt.exe" if is_win else "c3fmt"
    fmt_plat = "windows.exe" if is_win else ("macos" if is_mac else "linux")
    fmt_url = f"https://github.com/lmichaudel/c3fmt/releases/latest/download/c3fmt-{fmt_plat}"

    fmt_dest = stage / fmt_name
    download(fmt_url, fmt_dest)
    if fmt_dest.exists() and not is_win:
        fmt_dest.chmod(fmt_dest.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP)

    # 3. Compiler binary, PDB, and runtime
    shutil.copy2(c3c_bin, stage / c3c_bin.name)
    pdb = build_dir / "c3c.pdb"
    if pdb.exists():
        shutil.copy2(pdb, stage / "c3c.pdb")

    c3c_rt = build_dir / "c3c_rt"
    if c3c_rt.exists():
        shutil.copytree(c3c_rt, stage / "c3c_rt")

    # 4. Archive creation
    out_archive = root_dir / f"{args.out}.{args.format if args.format == 'zip' else 'tar.gz'}"
    if out_archive.exists():
        out_archive.unlink()

    if args.format == "zip":
        with zipfile.ZipFile(
            out_archive, "w", compression=zipfile.ZIP_DEFLATED
        ) as z:
            for p in stage.parent.rglob("*"):
                z.write(p, p.relative_to(stage.parent))
    else:
        with tarfile.open(out_archive, "w:gz") as t:
            t.add(stage, arcname="c3")

    shutil.rmtree(stage.parent)
    print(f"Package created: {out_archive}")


if __name__ == "__main__":
    sys.exit(main())