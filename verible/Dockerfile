FROM ubuntu:20.04
LABEL maintainer="Xan Johnson <xanjohns@gmail.com>"
ENV LANG en_US.UTF-8
#Install prerequisites
RUN \
  apt-get update &&\
  apt-get install -y curl grep wget
#Install verible
RUN \
  curl -s https://api.github.com/repos/google/verible/releases/latest |\
  grep "Ubuntu-20.04" |\
  grep "browser_download_url" |\
  cut -d : -f 2,3 |\
  xargs wget -q &&\
  tar -xf verible* &&\
  mv verible*/bin/verible-verilog-format /usr/bin &&\
  rm -r verible*
RUN mkdir -p /code
WORKDIR /code
CMD ["verible-verilog-format", "--helpfull"]
