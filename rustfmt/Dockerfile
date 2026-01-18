FROM rust:alpine3.23
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
ENV RUSTUP_TOOLCHAIN stable
RUN rustup component add rustfmt
WORKDIR /code
ENTRYPOINT []
CMD ["rustfmt", "--help"]
