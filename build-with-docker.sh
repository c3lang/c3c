#!/bin/bash
## build-with-docker.sh
## @author gdm85
## @modified by Kenta
##
## Script to build c3c for Ubuntu 22
##
#

read -p "Select Build Type: Debug/Release: " config

set -e

DOCKER=docker
DOCKER_RUN=""
IMAGE="c3c-builder"
if type podman 2>/dev/null >/dev/null; then
    DOCKER=podman
    DOCKER_RUN="--userns=keep-id"
    IMAGE="localhost/$IMAGE"
fi

if [ $config == "Debug" ]; then
    CMAKE_BUILD_TYPE=Debug
else
    CMAKE_BUILD_TYPE="$config"
fi

UBUNTU_VERSION="22.10"
LLVM_VERSION="15"

IMAGE="$IMAGE:22"

cd docker && $DOCKER build -t $IMAGE\
    --build-arg DEPS="llvm-$LLVM_VERSION-dev liblld-$LLVM_VERSION-dev clang-$LLVM_VERSION libllvm$LLVM_VERSION llvm-$LLVM_VERSION-runtime" \
    --build-arg UBUNTU_VERSION="$UBUNTU_VERSION" .
cd ..

rm -rf build bin
mkdir -p build bin

exec $DOCKER run -ti --rm --tmpfs=/tmp $DOCKER_RUN -v "$PWD":/home/c3c/source -w /home/c3c/source $IMAGE bash -c \
    "cd build && cmake -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE -DC3_LLVM_VERSION=$LLVM_VERSION .. && cmake --build . && mv c3c lib ../bin/"
