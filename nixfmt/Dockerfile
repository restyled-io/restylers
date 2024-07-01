FROM nixos/nix:2.23.1
LABEL maintainer="Chris Martin <ch.martin@gmail.com>"
RUN echo "experimental-features = nix-command flakes" > /etc/nix/nix.conf
RUN nix profile install --impure 'github:NixOS/nixfmt?rev=698954723ecec3f91770460ecae762ce590f2d9e'
RUN nix profile install --impure 'nixpkgs#gnused'
RUN mkdir -p /code
WORKDIR /code
CMD ["nixfmt", "--help"]
