
ARG UBUNTU_VERSION
FROM ubuntu:$UBUNTU_VERSION

ARG DEPS
ARG LLVM_VERSION
ARG UBUNTU_CODENAME

RUN apt-get update && \
    apt-get install -y wget software-properties-common

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
    add-apt-repository "deb http://apt.llvm.org/${UBUNTU_CODENAME}/ llvm-toolchain-${UBUNTU_CODENAME}-${LLVM_VERSION} main" && \
    apt-get update && \
    apt-get install -y build-essential cmake zlib1g zlib1g-dev curl \
    llvm-${LLVM_VERSION}-dev liblld-${LLVM_VERSION}-dev clang-${LLVM_VERSION} libllvm${LLVM_VERSION} llvm-${LLVM_VERSION}-runtime \
    ${DEPS} && \
    rm -rf /var/lib/apt/lists/*

RUN export DEBIAN_FRONTEND=noninteractive && export TERM=xterm && apt-get update && apt-get install -y build-essential cmake zlib1g zlib1g-dev

ARG GID=1000
ARG UID=1000

RUN groupadd -o --gid=$GID c3c && useradd --gid=$GID --uid=$GID --create-home --shell /bin/bash c3c

USER c3c
