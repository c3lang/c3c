
ARG UBUNTU_VERSION
FROM ubuntu:$UBUNTU_VERSION

ARG DEPS

RUN export DEBIAN_FRONTEND=noninteractive && export TERM=xterm && apt-get update && apt-get install -y build-essential cmake zlib1g zlib1g-dev  \
        $DEPS && \
	rm -rf /var/lib/apt/lists/*

ARG GID=1000
ARG UID=1000

RUN groupadd --gid=$GID c3c && useradd --gid=$GID --uid=$GID --create-home --shell /bin/bash c3c

USER c3c
