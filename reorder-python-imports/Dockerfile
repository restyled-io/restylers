FROM python:3.8-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN pip install reorder-python-imports==1.6.0
WORKDIR /code
ENTRYPOINT []
CMD ["reorder-python-imports", "--help"]
