IMAGES := $(wildcard */Dockerfile)

OVERRIDES := prettier-markdown

all: $(IMAGES:%=%.pushed) $(OVERRIDES:%=%.tested) restylers.yaml

restylers.yaml: */info.yaml
	./build/restyler-meta dump > $@

%/Dockerfile.pushed: %/Dockerfile.tested %/info.yaml
	./build/restyler-meta get "$*" image | ./build/push-image
	echo > $@

%/Dockerfile.tested: %/Dockerfile.built test/%.t %/info.yaml
	cram --shell=bash "test/$*.t"
	echo > $@

%/Dockerfile.built: %/Dockerfile %/info.yaml
	docker build --tag "$$(./build/restyler-meta get "$*" image)" "$*"
	echo > $@

%.tested: %/info.yaml
	cram --shell=bash "test/$*.t"
	echo > $@

.PHONY: generate-travis
generate-travis:
	build/generate-travis

RELASE_TAG ?= $(shell date +'%Y%m%d')

.PHONY: release
release: all
	git add restylers.yaml
	git commit -m 'Update restylers.yaml' || true
	git tag -f -s -m "$(RELASE_TAG)" "$(RELASE_TAG)"
	git push --follow-tags

.SECONDARY:
