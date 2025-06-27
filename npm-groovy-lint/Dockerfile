FROM node:22.17-slim
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN mkdir -p /app
WORKDIR /app
COPY package.json .
RUN npm install
COPY npm-groovy-lint /usr/bin/npm-groovy-lint
RUN npm-groovy-lint --version
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["npm-groovy-lint", "--help"]
