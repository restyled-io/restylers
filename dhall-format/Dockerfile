FROM nixos/nix:2.30.2
LABEL maintainer="Roki <falgon53@gmail.com>"
RUN nix-env --install dhall
RUN mkdir -p /code
WORKDIR /code
CMD ["dhall", "--help"]
