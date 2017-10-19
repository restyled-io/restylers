ALL_RESTYLERS         ?= $(wildcard *)
RESTYLER_IMAGE_PREFIX ?= restyled/restyler-

all: restylers

.PHONY: restylers
restylers: $(ALL_RESTYLERS)
	@for r in $^; do \
	  if [ -f "$$r/Dockerfile" ]; then \
	    (cd "$$r" && \
	      docker build --tag "$(RESTYLER_IMAGE_PREFIX)$$r" .); \
	  fi; \
	done

.PHONY: restylers.release
restylers.release: $(ALL_RESTYLERS)
	@for r in $^; do \
	  if [ -f "$$r/Dockerfile" ]; then \
	    docker push "$(RESTYLER_IMAGE_PREFIX)$$r"; \
	  if; \
	done
