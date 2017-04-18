all: html examples docs-examples

.PHONY: html
html: docs/index.html

docs/index.html: docs/src/index.md docs/src/template.html docs/src/*.purs
	pandoc \
		$< \
		-t html5 \
		--standalone \
		-S \
		--filter pandoc-include-code \
		--toc \
		"--metadata=subtitle:$(VERSION)" \
		--template=docs/src/template.html \
		-o $@

.PHONY: examples
examples: bower_components
	pulp build -I examples

.PHONY: docs-examples
docs-examples: bower_components
	pulp build -I docs/src

bower_components:
	bower install
