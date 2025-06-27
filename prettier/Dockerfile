FROM node:22.17-slim
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN mkdir -p /app
WORKDIR /app
COPY package.json .
RUN yarn install && yarn cache clean
COPY install.sh /tmp
RUN /tmp/install.sh
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["prettier", "--help"]
