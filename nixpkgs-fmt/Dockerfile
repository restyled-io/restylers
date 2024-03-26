FROM nixos/nix:2.21.1
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN nix-env --install gnused nixpkgs-fmt
RUN mkdir -p /code
WORKDIR /code
CMD ["nixpkgs-fmt", "--help"]
