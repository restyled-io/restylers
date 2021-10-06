FROM python:3.10-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
COPY requirements.txt .
RUN pip install --no-cache-dir --requirement requirements.txt
WORKDIR /code
ENTRYPOINT []
CMD ["reorder-python-imports", "--help"]
