FROM python:3.9-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN pip install --no-cache-dir reorder-python-imports==2.3.6
WORKDIR /code
ENTRYPOINT []
CMD ["reorder-python-imports", "--help"]
