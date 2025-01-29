test +args:
  stack --stack-yaml _tools/stack.yaml run -- restylers --no-pull {{args}}

headroom:
  headroom run -r
  cd _tools && fourmolu -i ./restylers{,-docs}/{app,src,test}

docs:
  stack --stack-yaml _tools/stack.yaml exec -- restylers-docs > _docs/restylers.md
