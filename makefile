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
