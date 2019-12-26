FROM rust:alpine3.10
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
RUN cargo install shellharden --vers 4.1.1
WORKDIR /code
ENTRYPOINT []
CMD ["shellharden", "--help"]
