AWS ?= aws --profile restyled-ci
RELEASE_ENV ?= prod
RELEASE_TAG ?= $(shell date +'%Y-%m-%d.%s')

.PHONY: release
release: .released

.released: restylers.yaml
	git tag -f -s -m "$(RELEASE_TAG)" "$(RELEASE_TAG)"
	git push --follow-tags
	$(AWS) cloudformation update-stack \
	  --stack-name $(RELEASE_ENV)-services \
	  --use-previous-template \
	  --parameters \
	    "ParameterKey=Environment,UsePreviousValue=true" \
	    "ParameterKey=RestylerImage,UsePreviousValue=true" \
	    "ParameterKey=RestyledImage,UsePreviousValue=true" \
	    "ParameterKey=AppsWebhooksDesiredCount,UsePreviousValue=true" \
	    "ParameterKey=RestylersVersion,ParameterValue=$(RELEASE_TAG)" \
	  --capabilities CAPABILITY_NAMED_IAM
	aws cloudformation wait stack-update-complete \
	  --stack-name $(RELEASE_ENV)-services
	./build/make-available-restylers \
	  > ../restyled.io.wiki/Available-Restylers.md
	(cd ../restyled.io.wiki && \
	  git commit Available-Restylers.md -m "Update Available Restylers" && \
	  git pull --rebase && \
	  git push)

restylers.yaml: */info.yaml
	stack exec restylers -- --manifest $@ release $?
	./build/sort-yaml $@ \
	  enabled \
	  name \
	  image \
	  command \
	  arguments \
	  include \
	  interpreters \
	  supports_arg_sep \
	  supports_multiple_paths \
	  documentation
	restyle-path $@
	./build/check-commit 'Update $@' $@

# %/Dockerfile.linted: %/Dockerfile
# 	@build/hadolint-pretty "$*/Dockerfile"
# 	echo > $@

.PHONY: mark-released
mark-released:
	touch */info.yaml
	touch restylers.yaml
	touch .released
