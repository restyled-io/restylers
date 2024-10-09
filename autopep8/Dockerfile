FROM python:3.13-alpine
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG C.UTF-8
COPY requirements.txt .
RUN pip install --no-cache-dir --requirement requirements.txt
WORKDIR /code
ENTRYPOINT []
CMD ["autopep8", "--help"]
