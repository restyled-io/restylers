FROM nixos/nix:2.29.0
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN nix-env --install gnused nixpkgs-fmt
RUN mkdir -p /code
WORKDIR /code
CMD ["nixpkgs-fmt", "--help"]
