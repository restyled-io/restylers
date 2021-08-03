FROM ubuntu:20.04
LABEL maintainer="Xan Johnson <xanjohns@gmail.com>"
ENV LANG en_US.UTF-8
#Install prerequisites
RUN \
  apt-get update &&\
  apt-get install -y wget
#Install verible
ENV VERIBLE_VERSION v0.0-1318-gf6b4485
RUN \
  wget -q https://github.com/chipsalliance/verible/releases/download/$VERIBLE_VERSION/verible-$VERIBLE_VERSION-Ubuntu-20.04-focal-x86_64.tar.gz &&\
  tar -xf verible* &&\
  mv verible*/bin/verible-verilog-format /usr/bin &&\
  rm -r verible*
RUN mkdir -p /code
WORKDIR /code
CMD ["verible-verilog-format", "--helpfull"]
