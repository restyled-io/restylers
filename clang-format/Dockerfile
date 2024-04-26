FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN \
  apt-get update -y && \
  apt-get install -y --no-install-recommends ca-certificates gnupg2 wget && \
  { \
    echo "deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy-18 main"; \
    echo "deb-src http://apt.llvm.org/jammy/ llvm-toolchain-jammy-18 main"; \
  } >>/etc/apt/sources.list && \
  wget --quiet -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
  apt-get update -y && \
  apt-get install -y --no-install-recommends clang-format-18 && \
  rm -rf /var/lib/apt/lists/*
RUN ln -s /usr/bin/clang-format-18 /usr/bin/clang-format
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["clang-format", "--help"]
