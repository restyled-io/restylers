.DEFAULT_GOAL := manifest

# A simple text file of the image tags we most-recently pushed, to make it less
# error-prone to copy/paste into the Restyler code that uses it.
manifest: */Dockerfile.pushed
	cat $^ | sort > $@

# Evidence that we've pushed a tested image, built from the Dockerfile.
# This will refuse to run if the working directory is not clean, since that
# could be pushing over an existing tag.
%/Dockerfile.pushed: %/Dockerfile.tested
	image=restyled/restyler-$(shell dirname $<); \
	tag=$(shell git rev-parse --short HEAD); \
	./build/push-image "$$image" "$$tag" && \
	echo "$$image:$$tag" >$@

# Evidence that we've tested the image built from the Dockerfile. This will only
# run if the image is re-built or the test file itself changes.
%/Dockerfile.tested: %/Dockerfile.built
	cram test/$(shell dirname $<).t
	echo > $@

# Evidence that we've built the image from the Dockerfile. This will only run if
# the Dockerfile itself changes.
%/Dockerfile.built: %/Dockerfile
	restyler=$(shell dirname $<); \
	  docker build --tag restyled/restyler-$$restyler $$restyler
	echo > $@
