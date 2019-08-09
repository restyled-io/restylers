IMAGES := $(wildcard */Dockerfile)

all: $(IMAGES:%=%.pushed) restylers.yaml

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

.PHONY: generate-travis
generate-travis:
	build/generate-travis

RELASE_TAG ?= $(shell date +'%Y%m%d')

.PHONY: release
release: all
	git add restylers.yaml
	git commit -m 'Update restylers.yaml'
	git tag -s -m "$(RELASE_TAG)" "$(RELASE_TAG)"
	git push --follow-tags

.SECONDARY:
