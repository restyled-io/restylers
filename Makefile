.DEFAULT_GOAL := manifest

manifest: */Dockerfile.pushed
	cat $^ | sort > $@

%/Dockerfile.pushed: %/Dockerfile.tested
	image=restyled/restyler-$(shell dirname $<); \
	tag=$(shell git rev-parse --short HEAD); \
	./build/push-image "$$image" "$$tag" && \
	echo "$$image:$$tag" >$@

%/Dockerfile.tested: %/Dockerfile.built
	cram test/$(shell dirname $<).t
	echo > $@

%/Dockerfile.built: %/Dockerfile
	restyler=$(shell dirname $<); \
	docker build --tag restyled/restyler-$$restyler $$restyler
	echo > $@
