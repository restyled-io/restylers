FROM node:6.9.1
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
ENV PRETTIER_VERSION 1.14.2
RUN npm install -g "prettier@$PRETTIER_VERSION"
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["prettier", "--help"]
