FROM nixos/nix:2.3.6 as builder
LABEL maintainer="Roki <falgon53@gmail.com>"
RUN nix-env --install dhall
RUN mkdir -p /code
WORKDIR /code
CMD ["dhall", "--help"]
