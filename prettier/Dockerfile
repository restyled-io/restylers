FROM node:24.10-slim
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG=en_US.UTF-8
RUN mkdir -p /app
WORKDIR /app
COPY package.json .
RUN yarn install && yarn cache clean
COPY prettier-shim /usr/local/bin/prettier
COPY prettier-shim /usr/local/bin/prettier-with-tailwindcss
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["prettier", "--help"]
