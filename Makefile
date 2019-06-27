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
