AWS ?= aws --profile restyled
RELEASE_ENV ?= prod
RELEASE_TAG ?= $(shell date +'%Y%m%d')

.PHONY: release
release: restylers.yaml
	git tag -f -s -m "$(RELEASE_TAG)" "$(RELEASE_TAG)"
	git push --follow-tags
	$(AWS) ssm put-parameter \
	  --name /restyled/$(RELEASE_ENV)/restylers-version \
	  --type String \
	  --value "$(RELEASE_TAG)"
	./build/make-available-restylers \
	  > ../restyled.io.wiki/Available-Restylers.md
	(cd ../restyled.io.wiki && \
	  git commit Available-Restylers.md -m "Update Available Restylers" && \
	  git pull --rebase && \
	  git push)

restylers.yaml: */info.yaml
	stack exec restylers -- --manifest $@ release $?
	./build/sort-yaml $@ \
	  enabled \
	  name \
	  image \
	  command \
	  arguments \
	  include \
	  interpreters \
	  supports_arg_sep \
	  supports_multiple_paths \
	  documentation
	restyle-path $@
	#git commit -m 'Update restylers.yaml' $@

# %/Dockerfile.linted: %/Dockerfile
# 	@build/hadolint-pretty "$*/Dockerfile"
# 	echo > $@
