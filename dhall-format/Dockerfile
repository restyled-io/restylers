FROM nixos/nix:2.29.1
LABEL maintainer="Roki <falgon53@gmail.com>"
RUN nix-env --install dhall
RUN mkdir -p /code
WORKDIR /code
CMD ["dhall", "--help"]
