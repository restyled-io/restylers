IMAGES := $(wildcard */Dockerfile.pushed)

all: $(IMAGES) restylers.yaml

restylers.yaml: */info.yaml
	./build/restyler-meta dump > $@

%/Dockerfile.pushed: %/Dockerfile.tested
	./build/restyler-meta get "$*" image | ./build/push-image
	echo > $@

%/Dockerfile.tested: %/Dockerfile.built
	cram "test/$*.t"
	echo > $@

%/Dockerfile.built: %/Dockerfile %/info.yaml
	docker build --tag "$$(./build/restyler-meta get "$*" image)" "$*"
	echo > $@

RELASE_TAG ?= $(shell date +'%Y%m%d')

.PHONY: release
release: all
	git add restylers.yaml
	git commit -m 'Update restylers.yaml'
	git tag -s -m "$(RELASE_TAG)" "$(RELASE_TAG)"
	git push --follow-tags
