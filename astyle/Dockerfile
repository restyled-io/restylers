FROM debian:jessie as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
  apt-get install -y clang g++ locales make
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  locale-gen
ENV LANG en_US.UTF-8
WORKDIR /tmp
COPY astyle_3.1_linux.tar.gz .
RUN tar xvf astyle_3.1_linux.tar.gz && \
  cd astyle/build/clang && make

FROM debian:jessie
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y locales make
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen
ENV LANG en_US.UTF-8
COPY --from=builder /tmp/astyle/build/clang/bin/astyle /usr/bin/astyle
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["astyle", "--help"]
