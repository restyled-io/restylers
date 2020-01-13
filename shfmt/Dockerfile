FROM golang:alpine as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
RUN GO111MODULE=on go get mvdan.cc/sh/v3/cmd/shfmt
RUN find / -name 'shfmt'

FROM alpine
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
COPY --from=builder /go/bin/shfmt /usr/bin/shfmt
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["shfmt", "--help"]
