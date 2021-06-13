#!/bin/bash
## build-with-docker.sh
## @author gdm85
##
## Script to build c3c for either Ubuntu 18 or Ubuntu 20.
##
#

if [ $# -ne 1 -a $# -ne 2 ]; then
	echo "Usage: build-with-docker.sh (18|20) [Debug|Release]" 1>&2
	exit 1
fi

set -e

if [ -z "$2" ]; then
	CMAKE_BUILD_TYPE=Debug
else
	CMAKE_BUILD_TYPE="$2"
fi

if [ "$1" = 18 ]; then
	UBUNTU_VERSION="18.04"
	DEPS="llvm-10-dev liblld-10-dev libclang-10-dev"
elif [ "$1" = 20 ]; then
	UBUNTU_VERSION="20.04"
	DEPS="llvm-11-dev liblld-11-dev clang-11 libllvm11 llvm-11-runtime"
else
	echo "ERROR: expected 18 or 20 as Ubuntu version argument" 1>&2
	exit 2
fi

cd docker && docker build -t c3c-builder --build-arg UID=$(id -u) --build-arg GID=$(id -g) \
	--build-arg DEPS="$DEPS" --build-arg UBUNTU_VERSION="$UBUNTU_VERSION" .
cd ..

rm -rf build bin
mkdir -p build bin

exec docker run -ti --rm -v "$PWD":/home/c3c/source -w /home/c3c/source c3c-builder bash -c \
	"cd build && cmake -DLLVM_DIR=/usr/lib/llvm-11/cmake -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE .. && cmake --build . && mv c3c ../bin/"
