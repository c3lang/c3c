#!/usr/bin/env python3
import argparse
import os
import shutil
import sys
import tarfile
import urllib.request
import time
from pathlib import Path


def is_alpine():
    return Path("/etc/alpine-release").exists()


def get_platform_id(system, cpu, is_static):
    sys_map = {
        "linux": "linux",
        "darwin": "darwin",
        "windows": "windows",
        "emscripten": "wasm32-emscripten",
    }
    cpu_map = {
        "x86_64": "amd64",
        "amd64": "amd64",
        "aarch64": "aarch64",
        "arm64": "aarch64",
        "riscv64": "riscv64",
    }
    os_name = sys_map.get(system.lower(), system.lower())
    if os_name == "wasm32-emscripten":
        return os_name
    arch = cpu_map.get(cpu.lower(), cpu.lower())
    if os_name == "linux" and arch == "amd64" and (is_static or is_alpine()):
        return "linux-amd64-musl"
    return f"{os_name}-{arch}"


def get_terminal_stream():
    try:
        if sys.platform == "win32":
            return open("CONOUT$", "w")
        return open("/dev/tty", "w")
    except OSError:
        return sys.stderr


def download_with_progress(url, dest_file):
    term = get_terminal_stream()
    req = urllib.request.Request(
        url, headers={"User-Agent": "c3c-meson-fetcher"}
    )
    try:
        with urllib.request.urlopen(req) as resp, open(dest_file, "wb") as out:
            total_len = resp.headers.get("Content-Length")
            total_bytes = int(total_len) if total_len and total_len.isdigit() else 0
            downloaded = 0
            chunk_size = 1024 * 128
            bar_width = 30

            while True:
                chunk = resp.read(chunk_size)
                if not chunk:
                    break
                out.write(chunk)
                downloaded += len(chunk)
                if total_bytes:
                    pct = downloaded / total_bytes
                    filled = int(bar_width * pct)
                    bar = "=" * filled + (">" if filled < bar_width else "")
                    bar = bar.ljust(bar_width, " ")
                    term.write(
                        f"\rFetching LLVM: [{bar}] {downloaded / 1048576:.1f}/{total_bytes / 1048576:.1f} MB ({pct * 100:.0f}%)"
                    )
                else:
                    term.write(f"\rFetching LLVM: {downloaded / 1048576:.1f} MB")
                term.flush()
            term.write("\n")
            term.flush()

            if total_bytes and downloaded < total_bytes:
                raise OSError(f"Download incomplete: got {downloaded} of {total_bytes} bytes")
    except Exception:
        if dest_file.exists():
            dest_file.unlink()
        raise


def copy_sanitizers(llvm_root, rt_dest):
    rt_out = Path(rt_dest)
    for pattern in ("*clang_rt.asan*", "*clang_rt.tsan*"):
        for f in llvm_root.glob(f"**/{pattern}"):
            if f.is_file():
                rt_out.mkdir(parents=True, exist_ok=True)
                shutil.copy2(f, rt_out)


def normalize_tree(llvm_root):
    if (llvm_root / "include").exists() and (llvm_root / "lib").exists():
        return
    inc_matches = list(llvm_root.rglob("include"))
    if inc_matches:
        real_root = inc_matches[0].parent
        if real_root != llvm_root:
            for item in list(real_root.iterdir()):
                target = llvm_root / item.name
                if not target.exists():
                    shutil.move(str(item), str(llvm_root))


def write_component_index(llvm_root):
    lib_dir = llvm_root / "lib"
    if not lib_dir.exists():
        return
    flags = []
    for f in sorted(lib_dir.glob("libLLVM*.a")):
        flags.append(f"-l{f.stem[3:]}")
    for f in sorted(lib_dir.glob("LLVM*.lib")):
        if f.stem in ("LLVM-C", "LLVM"):
            continue
        flags.append(f.name)
    (llvm_root / "llvm_link_flags.txt").write_text(" ".join(flags))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--system", required=True)
    parser.add_argument("--cpu", required=True)
    parser.add_argument("--buildtype", default="release")
    parser.add_argument("--tag", default="latest")
    parser.add_argument("--rt-dest", default="")
    parser.add_argument("--static", action="store_true")
    args = parser.parse_args()

    platform_id = get_platform_id(args.system, args.cpu, args.static)

    cache_base = os.environ.get("XDG_CACHE_HOME")
    if not cache_base:
        if args.system.lower() == "windows":
            cache_base = os.environ.get(
                "LOCALAPPDATA", str(Path.home() / "AppData" / "Local")
            )
        else:
            cache_base = str(Path.home() / ".cache")

    suffix = (
        "-dbg"
        if args.buildtype.lower() == "debug"
        and args.system.lower() != "emscripten"
        else ""
    )
    artifact_name = f"llvm-{platform_id}{suffix}"

    llvm_root = Path(cache_base) / "c3c" / "llvm" / f"{args.tag}_{artifact_name}"
    tarball = Path(cache_base) / "c3c" / "llvm" / f"{artifact_name}.tar.xz"
    stamp_file = llvm_root / ".stamp"

    term = get_terminal_stream()

    if (
        stamp_file.exists()
        and stamp_file.read_text().strip() == artifact_name
        and (llvm_root / "include").exists()
        and (llvm_root / "llvm_link_flags.txt").exists()
    ):
        if args.rt_dest:
            copy_sanitizers(llvm_root, args.rt_dest)
        print(str(llvm_root))
        return 0

    llvm_root.parent.mkdir(parents=True, exist_ok=True)

    if not tarball.exists() or tarball.stat().st_size == 0:
        url = f"https://github.com/c3lang/llvm-for-c3/releases/{'latest/download' if args.tag == 'latest' else f'download/{args.tag}'}/{artifact_name}.tar.xz"
        for attempt in range(1, 4):
            try:
                download_with_progress(url, tarball)
                break
            except Exception as e:
                if attempt == 3:
                    raise
                term.write(f"Download attempt {attempt}/3 failed: {e}. Retrying in 5s...\n")
                term.flush()
                time.sleep(5)

    term.write(f"Extracting LLVM to {llvm_root}...\n")
    term.flush()
    if llvm_root.exists():
        shutil.rmtree(llvm_root)
    llvm_root.mkdir(parents=True, exist_ok=True)

    try:
        with tarfile.open(tarball, "r:xz") as tar:
            try:
                tar.extractall(llvm_root, filter="data")
            except TypeError:
                tar.extractall(llvm_root)
    except Exception:
        if tarball.exists():
            tarball.unlink()
        if llvm_root.exists():
            shutil.rmtree(llvm_root)
        raise

    normalize_tree(llvm_root)
    write_component_index(llvm_root)

    if args.rt_dest:
        copy_sanitizers(llvm_root, args.rt_dest)

    stamp_file.write_text(artifact_name)
    print(str(llvm_root))
    return 0


if __name__ == "__main__":
    sys.exit(main())