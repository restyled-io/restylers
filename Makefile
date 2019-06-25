restylers.yaml: */info.yaml
	./build/restyler-meta dump > $@

%/Dockerfile.pushed: %/Dockerfile.tested
	name=$(shell dirname $<); \
	./build/restyler-meta get "$*" image | ./build/push-image
	echo > $@

%/Dockerfile.tested: %/Dockerfile.built
	cram test/$(shell dirname $<).t
	echo > $@

%/Dockerfile.built: %/Dockerfile %/info.yaml
	name=$(shell dirname $<); \
	image=$(shell ./build/restyler-meta get "$$name" image) && \
	docker build --tag "$$image" "$$name"
	echo > $@
