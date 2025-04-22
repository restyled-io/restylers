FROM ubuntu:24.04
LABEL maintainer="Xan Johnson <xanjohns@gmail.com>"
ENV LANG en_US.UTF-8
#Install prerequisites
RUN \
  apt-get update &&\
  apt-get install \
    --yes \
    --no-install-recommends \
      ca-certificates \
      curl &&\
  apt-get clean &&\
  rm -rf /var/lib/apt/lists/*
#Install verible
ENV VERIBLE_VERSION v0.0-3969-g6a70680a
RUN \
  curl --silent --fail --show-error --location \
    https://github.com/chipsalliance/verible/releases/download/$VERIBLE_VERSION/verible-$VERIBLE_VERSION-linux-static-x86_64.tar.gz | \
  tar -xzf - &&\
  mv verible*/bin/verible-verilog-format /usr/bin &&\
  rm -r verible*
RUN mkdir -p /code
WORKDIR /code
CMD ["verible-verilog-format", "--helpfull"]
