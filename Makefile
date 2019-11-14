IMAGES := $(wildcard */Dockerfile)

OVERRIDES := $(shell ./build/restyler-meta overrides)

all: $(IMAGES:%=%.pushed) $(OVERRIDES:%=%.tested) restylers.yaml

restylers.yaml: */info.yaml
	./build/restyler-meta dump > $@
	restyle-path $@

%/Dockerfile.pushed: %/Dockerfile.tested %/info.yaml
	./build/restyler-meta get "$*" image | ./build/push-image
	echo > $@

%/Dockerfile.tested: %/Dockerfile.built %/info.yaml
	rspec --tag "$*"
	echo > $@

%/Dockerfile.built: %/Dockerfile %/info.yaml
	docker build --tag "$$(./build/restyler-meta get "$*" image)" "$*"
	echo > $@

%.tested: %/info.yaml
	rspec --tag "$*"
	echo > $@

.PHONY:
wiki:
	./build/make-available-restylers \
	  > ../restyled.io.wiki/Available-Restylers.md
	(cd ../restyled.io.wiki && \
	  git commit Available-Restylers.md -m "Update Available Restylers" && \
	  git push)

RELASE_TAG ?= $(shell date +'%Y%m%d')

.PHONY: release_tag
release_tag: all
	git add restylers.yaml
	git commit -m 'Update restylers.yaml' || true
	git tag -f -s -m "$(RELASE_TAG)" "$(RELASE_TAG)"
	git push --follow-tags

.PHONY: release
release: all release_tag wiki

.SECONDARY:
