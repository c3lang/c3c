#!/bin/bash

: ${DOCKER:=docker}
: ${IMAGE:="c3c-builder"}
: ${CMAKE_BUILD_TYPE:=Release}
: ${LLVM_VERSION:=18}
: ${UBUNTU_VERSION:="22.04"}
: ${CMAKE_VERSION:="3.20.0"}

cd docker || exit 1  # Exit if the 'docker' directory doesn't exist

$DOCKER build \
    --build-arg LLVM_VERSION=$LLVM_VERSION \
    --build-arg CMAKE_VERSION=$CMAKE_VERSION \
    --build-arg UBUNTU_VERSION=$UBUNTU_VERSION \
    -t $IMAGE .

if [ $? -ne 0 ]; then
    echo "Docker image build failed. Exiting."
    exit 1
fi

cd ..

rm -rf build bin
mkdir -p build bin

chmod -R 777 build bin

exec $DOCKER run -i --rm \
    -v "$PWD":/home/c3c/source \
    -w /home/c3c/source $IMAGE bash -c \
    "cmake -S . -B build \
            -G Ninja \
            -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE \
            -DCMAKE_C_COMPILER=clang-$LLVM_VERSION \
            -DCMAKE_CXX_COMPILER=clang++-$LLVM_VERSION \
            -DCMAKE_LINKER=lld-$LLVM_VERSION \
            -DCMAKE_OBJCOPY=llvm-objcopy-$LLVM_VERSION \
            -DCMAKE_STRIP=llvm-strip-$LLVM_VERSION \
            -DCMAKE_DLLTOOL=llvm-dlltool-$LLVM_VERSION \
            -DC3_LLVM_VERSION=auto && \
    cmake --build build && \
    cp -r build/c3c build/lib bin"