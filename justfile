test +args:
  stack --stack-yaml _tools/stack.yaml run -- restylers --no-pull {{args}}

lint:
  docker run --volume "$PWD:/src:ro" --workdir /src \
    hadolint/hadolint hadolint \
    --config ./.hadolint.yaml \
    --failure-threshold error \
    ./*/Dockerfile
