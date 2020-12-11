FROM node:12.17-slim
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
ENV PRETTIER_VERSION 2.2.1
RUN npm install -g "prettier@$PRETTIER_VERSION"
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["prettier", "--help"]
