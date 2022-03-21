#!/bin/bash
## build-with-docker.sh
## @author gdm85
##
## Script to build c3c for either Ubuntu 20, 21 or 22.
##
#

if [ $# -ne 1 -a $# -ne 2 ]; then
	echo "Usage: build-with-docker.sh (20|21|22) [Debug|Release]" 1>&2
	exit 1
fi

set -e

if [ -z "$2" ]; then
	CMAKE_BUILD_TYPE=Debug
else
	CMAKE_BUILD_TYPE="$2"
fi

TAG="$1"
if [ "$1" = 20 ]; then
	UBUNTU_VERSION="20.04"
	LLVM_VERSION="12"
elif [ "$1" = 21 ]; then
	UBUNTU_VERSION="21.10"
	LLVM_VERSION="13"
elif [ "$1" = 22 ]; then
	UBUNTU_VERSION="22.04"
	LLVM_VERSION="14"
else
	echo "ERROR: expected 20, 21 or 22 as Ubuntu version argument" 1>&2
	exit 2
fi

cd docker && docker build -t c3c-builder:$TAG --build-arg UID=$(id -u) --build-arg GID=$(id -g) \
	--build-arg DEPS="llvm-$LLVM_VERSION-dev liblld-$LLVM_VERSION-dev clang-$LLVM_VERSION libllvm$LLVM_VERSION llvm-$LLVM_VERSION-runtime" \
	--build-arg UBUNTU_VERSION="$UBUNTU_VERSION" .
cd ..

rm -rf build bin
mkdir -p build bin

exec docker run -ti --rm -v "$PWD":/home/c3c/source -w /home/c3c/source c3c-builder:$TAG bash -c \
	"cd build && cmake -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE -DC3_LLVM_VERSION=$LLVM_VERSION .. && cmake --build . && mv c3c lib ../bin/"
