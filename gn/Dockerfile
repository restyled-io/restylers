FROM ubuntu:20.04
LABEL maintainer="Michael Spang <spang@google.com"
ENV LANG en_US.UTF-8
RUN apt-get update && \
    apt-get install -y wget unzip
RUN wget -O gn-latest.zip https://chrome-infra-packages.appspot.com/dl/gn/gn/linux-amd64/+/latest && \
    unzip -d /usr/local/bin gn-latest.zip gn && \
    rm gn-latest.zip
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["gn", "--help"]
