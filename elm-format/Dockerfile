FROM node:9.9.0-slim
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
ENV ELM_VERSION 0.18
ENV ELM_FORMAT_VERSION 0.6.1-alpha
RUN yarn global add elm@0.18 elm-format@$ELM_FORMAT_VERSION
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["elm-format", "--help"]
