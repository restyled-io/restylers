FROM ocaml/opam:ubuntu-22.04
LABEL maintainer="Pat Brisbin <pbrisbin@restyled.io>"
RUN opam install ocamlformat

# hadolint ignore=DL3002
USER root
RUN cp -r /home/opam/.opam /root/.opam
RUN ln -s /root/.opam/*/bin/ocamlformat /usr/local/bin/ocamlformat
RUN ocamlformat --version

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["ocamlformat", "--help"]
