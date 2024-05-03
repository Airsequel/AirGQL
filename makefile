.PHONY: test
test:
	stack \
		--stack-yaml stack-standalone.yaml \
		test


.PHONY: install
install:
	stack \
		--stack-yaml stack-standalone.yaml \
		install


.PHONY: docs
docs:
	stack \
		--stack-yaml stack-standalone.yaml \
		haddock \
		--haddock-for-hackage


.PHONY: release
release: docs
	stack \
		--stack-yaml stack-standalone.yaml \
		upload \
		.
	stack \
		--stack-yaml stack-standalone.yaml \
		upload \
		--documentation \
		.
