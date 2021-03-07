.PHONY: repl
repl:
	npx spago repl

.PHONY: watch
watch: ## Compile files on change
	@npx spago build --watch

.PHONY: clean
clean: ## Remove all generated project files (keeps standard library).
	@find src -type f | cut -d/ -f2- | cut -d. -f1 | sed 's/\//./g' | sed 's/^/output\//' | xargs -L1 rm -rf
	@rm -rf output/*
	@rm -rf output/.s

tags: src
	@npx spago docs -f ctags

.PHONY: build
build: output/.s

output/.s: src
	npx spago build
	@touch output/.s

.PHONY: images
images: dist/.images

dist/.images: data
	mkdir -p dist
	cp -f data/* dist/
	@touch dist/.images

.PHONY: dist
dist: dist/.s

dist/.s: output/.s dist/.images
	npx parcel build src/index/index.html --public-url './'
	@touch dist/.s

.PHONY: serve
serve: output/.s
	npx parcel src/index/index.html
