FROM nixos/nix:2.32.1
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN echo "experimental-features = nix-command flakes" > /etc/nix/nix.conf
RUN nix profile install --impure 'github:NixOS/nixpkgs?rev=c8aa8cc00a5cb57fada0851a038d35c08a36a2bb#nodePackages.prettier'
RUN nix profile install --impure 'nixpkgs#gnused'
RUN mkdir -p /code
WORKDIR /code
CMD ["prettier", "--help"]
