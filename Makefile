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
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name $(RELEASE_ENV)-services
	./build/make-available-restylers \
	  > ../restyled.io.wiki/Available-Restylers.md
	(cd ../restyled.io.wiki && \
	  git commit Available-Restylers.md -m "Update Available Restylers" && \
	  git pull --rebase && \
	  git push)

restylers.yaml: restylers/*/info.yaml
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

.PHONY: mark-released
mark-released:
	touch restylers/*/info.yaml
	touch restylers.yaml
	touch .released

.PHONY: image.build
image.build:
	docker build --tag restyled/restylers .

.PHONY: image.push
image.push:
	docker tag restyled/restylers restyled/restylers:v1
	docker push restyled/restylers:v1
