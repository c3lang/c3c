ARG UBUNTU_VERSION=22.04
FROM ubuntu:${UBUNTU_VERSION}

ARG LLVM_VERSION=18
ENV LLVM_DEV_VERSION=20

ARG CMAKE_VERSION=3.20

RUN apt-get update && apt-get install -y wget gnupg software-properties-common zlib1g zlib1g-dev python3 ninja-build curl g++ && \
    wget https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-$CMAKE_VERSION-linux-x86_64.sh && \
    mkdir -p /opt/cmake && \
    sh cmake-${CMAKE_VERSION}-linux-x86_64.sh --prefix=/opt/cmake --skip-license && \
    rm cmake-${CMAKE_VERSION}-linux-x86_64.sh && \
    ln -s /opt/cmake/bin/cmake /usr/local/bin/cmake

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
    if [ "${LLVM_VERSION}" -lt 18 ]; then \
        add-apt-repository "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-${LLVM_VERSION} main" && \
        apt-get update && \
        apt-get install -y -t llvm-toolchain-focal-${LLVM_VERSION} \
          libpolly-${LLVM_VERSION}-dev \
          clang-${LLVM_VERSION} llvm-${LLVM_VERSION} llvm-${LLVM_VERSION}-dev \
          lld-${LLVM_VERSION} liblld-${LLVM_VERSION}-dev libmlir-${LLVM_VERSION} \
          libmlir-${LLVM_VERSION}-dev mlir-${LLVM_VERSION}-tools; \
    elif [ "${LLVM_VERSION}" -lt "${LLVM_DEV_VERSION}" ]; then \
        add-apt-repository "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-${LLVM_VERSION} main" && \
        apt-get update && \
        apt-get install -y -t llvm-toolchain-focal-${LLVM_VERSION} \
          libpolly-${LLVM_VERSION}-dev \
          clang-${LLVM_VERSION} clang++-${LLVM_VERSION} llvm-${LLVM_VERSION} llvm-${LLVM_VERSION}-dev \
          lld-${LLVM_VERSION} liblld-${LLVM_VERSION}-dev; \
    else \
        add-apt-repository "deb http://apt.llvm.org/focal/ llvm-toolchain-focal main" && \
        apt-get update && \
        apt-get install -y -t llvm-toolchain-focal \
          libpolly-${LLVM_VERSION}-dev \
          clang-${LLVM_VERSION} llvm-${LLVM_VERSION} llvm-${LLVM_VERSION}-dev \
          lld-${LLVM_VERSION} liblld-${LLVM_VERSION}-dev; \
    fi && \
    rm -rf /var/lib/apt/lists/*

RUN groupadd -g 1337 c3c && \
    useradd -m -u 1337 -g c3c c3c

# Add cmake to PATH for user c3c
USER c3c
ENV PATH="/opt/cmake/bin:${PATH}"

WORKDIR /home/c3c