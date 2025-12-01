test +args:
  stack --stack-yaml _tools/stack.yaml run -- restylers --no-pull {{args}}

sh restyler:
  docker run --rm -it public.ecr.aws/restyled-io/restyler-{{restyler}}:dev /bin/sh

bash restyler:
  docker run --rm -it public.ecr.aws/restyled-io/restyler-{{restyler}}:dev /bin/bash

headroom:
  headroom run -r
  cd _tools && fourmolu -i ./restylers{,-docs}/{app,src,test}

docs:
  stack --stack-yaml _tools/stack.yaml exec -- restylers-docs > _docs/restylers.md
