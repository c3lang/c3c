#!/usr/bin/env python3

# Script based on https://gist.github.com/mmozeiko/7f3162ec2988e81e56d5c4e22cde9977
# by Mārtiņš Možeiko.
# Changes and additions to the gist are licensed under the CC0 License.

import platform
import io
import json
import shutil
import hashlib
import zipfile
import argparse
import subprocess
import urllib.request
import os
import ssl
import tempfile
from pathlib import Path

if (platform.system() == "Windows"):
	print("Creating msvc_sdk for compilation without VS.")
else:
	print("Creating msvc_sdk for cross platform compilation to Windows.")

OUTPUT = Path(tempfile.mkdtemp()) # output folder
SDK_OUTPUT = Path("msvc_sdk")

if (not os.environ.get('PYTHONHTTPSVERIFY', '') and
		getattr(ssl, '_create_unverified_context', None)):
	ssl._create_default_https_context = ssl._create_unverified_context

MANIFEST_URL = "https://aka.ms/vs/17/release/channel"

def download(url):
	with urllib.request.urlopen(url) as res:
		return res.read()

def download_progress(url, check, name, f):
	data = io.BytesIO()
	with urllib.request.urlopen(url) as res:
		total = int(res.headers["Content-Length"])
		size = 0
		while True:
			block = res.read(1<<20)
			if not block:
				break
			f.write(block)
			data.write(block)
			size += len(block)
			perc = size * 100 // total
			print(f"\r{name} ... {perc}%", end="")
	print()
	data = data.getvalue()
	digest = hashlib.sha256(data).hexdigest()
	if check.lower() != digest:
		exit(f"Hash mismatch for f{pkg}")
	return data

# super crappy msi format parser just to find required .cab files
def get_msi_cabs(msi):
	index = 0
	while True:
		index = msi.find(b".cab", index+4)
		if index < 0:
			return
		yield msi[index-32:index+4].decode("ascii")

def first(items, cond):
	return next(item for item in items if cond(item))

### parse command-line arguments

ap = argparse.ArgumentParser()
ap.add_argument("--show-versions", const=True, action="store_const", help="Show available MSVC and Windows SDK versions")
ap.add_argument("--accept-license", const=True, action="store_const", help="Automatically accept license")
ap.add_argument("--msvc-version", help="Get specific MSVC version")
ap.add_argument("--sdk-version", help="Get specific Windows SDK version")
args = ap.parse_args()


### get main manifest

manifest = json.loads(download(MANIFEST_URL))


### download VS manifest

vs = first(manifest["channelItems"], lambda x: x["id"] == "Microsoft.VisualStudio.Manifests.VisualStudio")
payload = vs["payloads"][0]["url"]

vsmanifest = json.loads(download(payload))


### find MSVC & WinSDK versions

packages = {}
for p in vsmanifest["packages"]:
	packages.setdefault(p["id"].lower(), []).append(p)

msvc = {}
sdk_path = {}

for pid,p in packages.items():
	if pid.startswith("Microsoft.VisualStudio.Component.VC.".lower()) and pid.endswith(".x86.x64".lower()):
		pver = ".".join(pid.split(".")[4:6])
		if pver[0].isnumeric():
			msvc[pver] = pid
	elif pid.startswith("Microsoft.VisualStudio.Component.Windows10SDK.".lower()) or \
			 pid.startswith("Microsoft.VisualStudio.Component.Windows11SDK.".lower()):
		pver = pid.split(".")[-1]
		if pver.isnumeric():
			sdk_path[pver] = pid

if args.show_versions:
	print("MSVC versions:", " ".join(sorted(msvc.keys())))
	print("Windows SDK versions:", " ".join(sorted(sdk_path.keys())))
	exit(0)

msvc_ver = args.msvc_version or max(sorted(msvc.keys()))
sdk_ver = args.sdk_version or max(sorted(sdk_path.keys()))

if msvc_ver in msvc:
	msvc_pid = msvc[msvc_ver]
	msvc_ver = ".".join(msvc_pid.split(".")[4:-2])
else:
	exit(f"Unknown MSVC version: f{args.msvc_version}")

if sdk_ver in sdk_path:
	sdk_pid = sdk_path[sdk_ver]
else:
	exit(f"Unknown Windows SDK version: f{args.sdk_version}")

print(f"Downloading MSVC v{msvc_ver} and Windows SDK v{sdk_ver}")


### agree to license

tools = first(manifest["channelItems"], lambda x: x["id"] == "Microsoft.VisualStudio.Product.BuildTools")
resource = first(tools["localizedResources"], lambda x: x["language"] == "en-us")
license = resource["license"]

if not args.accept_license:
	accept = input(f"Do you accept Visual Studio license at {license}, and also confirm that you have a valid license Visual Studio license allowing you to download the VS Build Tools [Y/N] ?")
	if not accept or accept[0].lower() != "y":
		exit(0)

shutil.rmtree(SDK_OUTPUT, ignore_errors = True)
total_download = 0

### download Windows SDK

archs = [
	#"arm",
	#"arm64",
	"x64",
	#"x86"
]

msvc_packages = [
	f"microsoft.vc.{msvc_ver}.asan.headers.base",
]

for arch in archs:
	msvc_packages.append(f"microsoft.vc.{msvc_ver}.crt.{arch}.desktop.base")
	msvc_packages.append(f"microsoft.vc.{msvc_ver}.crt.{arch}.store.base")
	msvc_packages.append(f"microsoft.vc.{msvc_ver}.asan.{arch}.base")

for pkg in msvc_packages:
	p = first(packages[pkg], lambda p: p.get("language") in (None, "en-US"))
	for payload in p["payloads"]:
		with tempfile.TemporaryFile() as f:
			data = download_progress(payload["url"], payload["sha256"], pkg, f)
			total_download += len(data)
			with zipfile.ZipFile(f) as z:
				for name in z.namelist():
					if name.startswith("Contents/"):
						out = OUTPUT / Path(name).relative_to("Contents")
						out.parent.mkdir(parents=True, exist_ok=True)
						out.write_bytes(z.read(name))

sdk_packages = [
	# Windows SDK libs
	"Windows SDK for Windows Store Apps Libs-x86_en-us.msi",
	"Windows SDK Desktop Libs x64-x86_en-us.msi",
	# CRT headers & libs
	"Universal CRT Headers Libraries and Sources-x86_en-us.msi",
]

with tempfile.TemporaryDirectory() as d:
	dst = Path(d)

	sdk_pkg = packages[sdk_pid][0]
	sdk_pkg = packages[first(sdk_pkg["dependencies"], lambda x: True).lower()][0]

	msi = []
	cabs = []

	# download msi files
	for pkg in sdk_packages:
		payload = first(sdk_pkg["payloads"], lambda p: p["fileName"] == f"Installers\\{pkg}")
		msi.append(dst / pkg)
		with open(dst / pkg, "wb") as f:
			data = download_progress(payload["url"], payload["sha256"], pkg, f)
			total_download += len(data)
			cabs += list(get_msi_cabs(data))

	# download .cab files
	for pkg in cabs:
		payload = first(sdk_pkg["payloads"], lambda p: p["fileName"] == f"Installers\\{pkg}")
		with open(dst / pkg, "wb") as f:
			download_progress(payload["url"], payload["sha256"], pkg, f)

	print("Unpacking msi files...")

	# run msi installers
	for m in msi:
		if (platform.system() == "Windows"):
			subprocess.check_call(["msiexec.exe", "/a", m, "/quiet", "/qn", f"TARGETDIR={OUTPUT.resolve()}"])
		else:
			subprocess.check_call(["msiextract", m, '-C', OUTPUT.resolve()])


### versions

if (platform.system() == "Windows"):
	window_kit = OUTPUT / "Windows Kits/"
else:
	window_kit = OUTPUT / "Program Files/Windows Kits/"

ucrt = list(window_kit.glob("*/Lib/*/ucrt"))[0]
um = list(window_kit.glob("*/Lib/*/um"))[0]
lib = list((OUTPUT / "VC/Tools/MSVC/").glob("*/lib"))[0]

SDK_OUTPUT.mkdir(exist_ok=True)

def copy(src, dst):
	low = dst.lower()
	base = os.path.basename(low)
	if base == "msvcrt.lib" or base == "oldnames.lib":
		base = base[:-3].upper() + "lib"
		path = os.path.join(os.path.dirname(low), base);
		shutil.copy(src, path)
	shutil.copy(src, low)

for arch in archs:
	out_dir = SDK_OUTPUT / arch
	shutil.copytree(ucrt / arch, out_dir, copy_function=copy, dirs_exist_ok=True)
	shutil.copytree(um / arch, out_dir, copy_function=copy, dirs_exist_ok=True)
	shutil.copytree(lib / arch, out_dir, copy_function=copy, dirs_exist_ok=True)

print("Congratulations! The 'msvc_sdk' directory was successfully generated.")
