# https://www.stackage.org/lts-11.10/package/hindent-5.2.5
FROM fpco/stack-build:lts-11.10 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH
RUN stack --resolver lts-11.10 setup
RUN stack --resolver lts-11.10 install hindent

FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/hindent /usr/bin/hindent
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["hindent", "--help"]
