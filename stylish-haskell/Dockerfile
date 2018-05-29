FROM fpco/stack-build:lts-11.11 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH
RUN stack setup
RUN stack install stylish-haskell

FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/stylish-haskell /usr/bin/stylish-haskell
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["stylish-haskell", "--help"]
