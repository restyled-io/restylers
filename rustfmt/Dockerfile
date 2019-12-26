FROM rust:alpine3.10
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN rustup toolchain install nightly
RUN rustup component add rustfmt --toolchain nightly
RUN rustup default nightly
WORKDIR /code
ENTRYPOINT []
CMD ["rustfmt", "--help"]
