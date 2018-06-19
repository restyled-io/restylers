FROM golang:alpine as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
RUN apk add --no-cache git go
ENV SHFMT_SRC /usr/local/go/src/mvdan.cc/sh
RUN git clone https://github.com/mvdan/sh "$SHFMT_SRC"

# Just a few commits beyond v2.4.0 so we can get the -r option
# https://github.com/mvdan/sh/compare/v2.4.0...8b544f4441579a366bc2b646cceb9d3f5a70e17c
ENV SHFMT_REF 8b544f4441579a366bc2b646cceb9d3f5a70e17c
RUN \
  cd "$SHFMT_SRC" && \
  git reset --hard "$SHFMT_REF" && \
  go build mvdan.cc/sh/cmd/shfmt

FROM alpine
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
COPY --from=builder \
  /usr/local/go/src/mvdan.cc/sh/shfmt \
  /usr/bin/shfmt
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["shfmt", "--help"]
