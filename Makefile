.DEFAULT_GOAL := manifest

# Pull and write state files for all minifested images in the manifest, to
# initialize a build cache with the assumption everything's been built, tested,
# and pushed already.
.PHONY: setup
setup:
	echo "TODO"

# A simple text file of the image tags we most-recently pushed, to make it less
# error-prone to copy/paste into the Restyler code that uses it.
manifest: */Dockerfile.pushed
	cat $^ | sort > $@

# Evidence that we've pushed a tested image, built from the Dockerfile.
# This will refuse to run if the working directory is not clean, since that
# could be pushing over an existing tag.
%/Dockerfile.pushed: %/Dockerfile.tested
	restyler=$(shell dirname $<); \
	sha=$(shell git rev-parse --short HEAD); \
	restyler_latest=restyled/restyler-$$restyler; \
	restyler_tagged=restyled/restyler-$$restyler:$$sha; \
	  docker tag $$restyler_latest $$restyler_tagged && \
	  docker push $$restyler_tagged && \
	echo "$$restyler_tagged" > $@

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
