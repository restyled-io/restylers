FROM nixos/nix:2.23.1
LABEL maintainer="Chris Martin <ch.martin@gmail.com>"
RUN nix-env --install gnused nixfmt
RUN mkdir -p /code
WORKDIR /code
CMD ["nixfmt", "--help"]
