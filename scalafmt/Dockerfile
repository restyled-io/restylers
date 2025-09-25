FROM openjdk:25-slim
LABEL maintainer="Pat Brisbin <pbrisbin@restyled.io>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get --no-install-recommends -y install \
    ca-certificates \
    curl \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8

# Install Coursier
RUN \
  curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | \
  gzip -d > cs && \
  chmod +x ./cs && \
  mv ./cs /usr/local/bin/cs

# Setup Courseir, unattended
RUN yes | cs setup
ENV PATH=/root/.local/share/coursier/bin:$PATH

# Install scalafmt
ENV SCALAFMT_VERSION=3.7.10
RUN cs install "scalafmt:$SCALAFMT_VERSION"

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["scalafmt", "--help"]
