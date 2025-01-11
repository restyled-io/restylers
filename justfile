test +args:
  stack --stack-yaml _tools/stack.yaml run -- restylers --no-pull {{args}}

lint:
  docker run --volume "$PWD:/src:ro" --workdir /src \
    hadolint/hadolint hadolint \
    --config ./.hadolint.yaml \
    --failure-threshold error \
    ./*/Dockerfile

headroom:
  stack --stack-yaml _tools/stack.yaml exec -- headroom run -r
  stack --stack-yaml _tools/stack.yaml exec -- fourmolu -i _tools/restylers{,-docs}/{app,src,test}
