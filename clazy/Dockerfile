FROM ubuntu:plucky-20251001
LABEL maintainer="Pat Brisbin pbrisbin@restyled.io"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y --no-install-recommends install \
    ca-certificates \
    clang \
    clang-tools \
    cmake \
    g++ \
    git-core \
    libclang-dev \
    llvm-dev \
    make \
    ninja-build \
    locales \
    qt6-base-dev && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV CLAZY_VERSION=v1.16
RUN \
  cd /tmp && \
  git clone https://github.com/KDE/clazy && \
  cd clazy/ && \
  git reset --hard "$CLAZY_VERSION" && \
  cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Release -G Ninja && \
  cmake --build . && \
  cmake --build . --target install && \
  cd /tmp && \
  rm -rf ./clazy

COPY clazy-standalone-apply /usr/local/bin/clazy-standalone-apply
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["clazy-standalone-apply", "--help"]
