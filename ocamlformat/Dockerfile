FROM ocaml/opam:ubuntu-22.04
LABEL maintainer="Pat Brisbin <pbrisbin@restyled.io>"
RUN opam install ocamlformat

USER root
RUN cp -r /home/opam/.opam /root/.opam
ENV PATH=/root/.opam/5.0/bin:$PATH

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["ocamlformat", "--help"]
