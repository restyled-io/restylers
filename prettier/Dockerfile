FROM nixos/nix:2.32.1
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN echo "experimental-features = nix-command flakes" > /etc/nix/nix.conf

RUN mkdir /restyler-prettier
COPY restyler-prettier /restyler-prettier
RUN nix profile install '/restyler-prettier'

RUN mkdir /code
WORKDIR /code
CMD ["restyler-prettier", "--help"]
