AWS ?= aws --profile restyled-ci
ENV ?= prod

BUCKET = $(shell \
  $(AWS) cloudformation describe-stacks \
    --stack-name $(ENV)-docs \
    --query 'Stacks[*].Outputs[?OutputKey==`BucketName`].OutputValue' \
    --output text \
)

DISTRIBUTION_ID = $(shell \
  $(AWS) cloudformation describe-stacks \
    --stack-name $(ENV)-docs \
    --query 'Stacks[*].Outputs[?OutputKey==`DistributionId`].OutputValue' \
    --output text \
)

PREFIX = /data-files/restylers/manifests
DEV_MANIFEST = $(PREFIX)/dev/restylers.yaml
LTS_MANIFEST = $(PREFIX)/stable/restylers.yaml

.PHONY: release
release:
	stack exec restylers -- release \
	  --write /tmp/restylers.yaml restylers/*/info.yaml
	$(AWS) s3 cp \
	  --acl public-read \
	  --content-type text/plain \
	  /tmp/restylers.yaml \
	  s3://$(BUCKET)$(DEV_MANIFEST)
	$(AWS) cloudfront create-invalidation \
	  --distribution-id $(DISTRIBUTION_ID) \
	  --paths $(DEV_MANIFEST)

.PHONY: release.promote
release.promote:
	$(AWS) s3 cp \
	  --acl public-read \
	  --content-type text/plain \
	  s3://$(BUCKET)$(DEV_MANIFEST) \
	  s3://$(BUCKET)$(LTS_MANIFEST)
	$(AWS) cloudfront create-invalidation \
	  --distribution-id $(DISTRIBUTION_ID) \
	  --paths $(LTS_MANIFEST)

# TODO: Broken with git-tag to S3 manifest transition
.PHONY: wiki
wiki:
	./build/make-available-restylers \
	  > ../restyled.io.wiki/Available-Restylers.md
	(cd ../restyled.io.wiki && \
	  git commit Available-Restylers.md -m "Update Available Restylers" && \
	  git pull --rebase && \
	  git push)
