FROM nixos/nix:2.26.2
LABEL maintainer="Chris Martin <ch.martin@gmail.com>"
RUN echo "experimental-features = nix-command flakes" > /etc/nix/nix.conf
RUN nix profile install --impure 'github:NixOS/nixfmt?rev=83de1eceaae8a891ae52a3a2b82226540207309e'
RUN nix profile install --impure 'nixpkgs#gnused'
RUN mkdir -p /code
WORKDIR /code
CMD ["nixfmt", "--help"]
