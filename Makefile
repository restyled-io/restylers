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

RELEASE_TAG ?= $(shell date +'%Y%m%d')

.PHONY: release.tag
release.tag:
	git tag -f -s -m "$(RELEASE_TAG)" "$(RELEASE_TAG)"
	git push --follow-tags

# TODO: Give restyled-ci permissions
AWS ?= aws --profile restyled
RELEASE_ENV ?= prod

.PHONY: release.restyler
release.restyler:
	$(AWS) ssm put-parameter \
	  --name /restyled/$(RELEASE_ENV)/restylers-version \
	  --type String \
	  --value "$(RELEASE_TAG)"

.PHONY: release.wiki
release.wiki:
	./build/make-available-restylers \
	  > ../restyled.io.wiki/Available-Restylers.md
	(cd ../restyled.io.wiki && \
	  git commit Available-Restylers.md -m "Update Available Restylers" && \
	  git pull --rebase && \
	  git push)

# IMAGES := $(wildcard */Dockerfile)

# OVERRIDES := $(shell ./build/restyler-meta overrides)

# all: $(IMAGES:%=%.pushed) $(OVERRIDES:%=%.tested)

# lint: $(IMAGES:%=%.linted)

# restylers.yaml: */info.yaml
# 	./build/restyler-meta dump > $@
# 	restyle-path $@
# 	git commit -m 'Update restylers.yaml' $@

# %/Dockerfile.pushed: %/Dockerfile.tested %/info.yaml
# 	./build/restyler-meta get "$*" image | ./build/push-image
# 	echo > $@

# %/Dockerfile.tested: %/Dockerfile.built %/info.yaml
# 	rspec --tag "$*"
# 	echo > $@

# %/Dockerfile.linted: %/Dockerfile
# 	@build/hadolint-pretty "$*/Dockerfile"
# 	echo > $@

# %/Dockerfile.built: %/Dockerfile %/info.yaml
# 	image="$$(./build/restyler-meta get "$*" image)"; \
# 	  if build/image-exists "$$image"; then \
# 	    docker pull "$$image"; \
# 	  else \
# 	    docker build --tag "$$image" "$*"; \
# 	  fi
# 	echo > $@

# %.tested: %/info.yaml
# 	rspec --tag "$*"
# 	echo > $@


# .PHONY: release.prep
# release.prep: all restylers.yaml

# .PHONY: release.prepped
# release.prepped: release.tag release.wiki release.restyler

# .NOTPARALLEL: release
# .PHONY: release
# release: release.prep release.prepped

# .SECONDARY:
