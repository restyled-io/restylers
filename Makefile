IMAGES := $(wildcard */Dockerfile)

OVERRIDES := $(shell ./build/restyler-meta overrides)

all: $(IMAGES:%=%.pushed) $(OVERRIDES:%=%.tested)

restylers.yaml: */info.yaml
	./build/restyler-meta dump > $@
	restyle-path $@
	git commit -m 'Update restylers.yaml' $@

%/Dockerfile.pushed: %/Dockerfile.tested %/info.yaml
	./build/restyler-meta get "$*" image | ./build/push-image
	echo > $@

%/Dockerfile.tested: %/Dockerfile.built %/info.yaml
	rspec --tag "$*"
	echo > $@

%/Dockerfile.built: %/Dockerfile %/info.yaml
	image="$$(./build/restyler-meta get "$*" image)"; \
	  if build/image-exists "$$image"; then \
	    docker pull "$$image"; \
	  else \
	    docker build --tag "$$image" "$*"; \
	  fi
	echo > $@

%.tested: %/info.yaml
	rspec --tag "$*"
	echo > $@

RELEASE_TAG ?= $(shell date +'%Y%m%d')

.PHONY: release.tag
release.tag:
	git tag -f -s -m "$(RELEASE_TAG)" "$(RELEASE_TAG)"
	git push --follow-tags

.PHONY: release.wiki
release.wiki:
	./build/make-available-restylers \
	  > ../restyled.io.wiki/Available-Restylers.md
	(cd ../restyled.io.wiki && \
	  git commit Available-Restylers.md -m "Update Available Restylers" && \
	  git pull --rebase && \
	  git push)

.PHONY: release.restyler
release.restyler:
	$(MAKE) -C ../restyler restylers_version \
	  RESTYLERS_VERSION=$(RELEASE_TAG)


.PHONY: release.prep
release.prep: all restylers.yaml

.PHONY: release.prepped
release.prepped: release.tag release.wiki release.restyler

.NOTPARALLEL: release
.PHONY: release
release: release.prep release.prepped

.SECONDARY:
