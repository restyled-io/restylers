.PHONY: release.dev
release.dev:
	stack exec restylers -- release \
	  --write /tmp/restylers.yaml restylers/*/info.yaml
	(cd ../ops && ./tools/promote --file /tmp/restylers.yaml dev)
