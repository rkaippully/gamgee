dhall-files := $(shell find ./dhall -type f)
dhall-derived-files = package.yaml .hlint.yaml .stylish-haskell.yaml

.PHONY: build
build: gen-files
	stack build
	hlint .

.PHONY: gen-files
gen-files: dhall-to-yaml $(dhall-derived-files)

package.yaml: $(dhall-files)
	echo "./dhall/package.dhall" | dhall-to-yaml > $@

.hlint.yaml: $(dhall-files)
	echo "./dhall/hlint.dhall" | dhall-to-yaml --quoted > $@

.stylish-haskell.yaml: $(dhall-files)
	echo "./dhall/stylish-haskell.dhall" | dhall-to-yaml > $@

.PHONY: dhall-to-yaml
dhall-to-yaml:
	@command -v dhall-to-yaml >/dev/null 2>&1 || { echo >&2 "Please install dhall-to-yaml as mentioned at https://github.com/dhall-lang/dhall-lang/wiki/Getting-started%3A-Generate-JSON-or-YAML#installation. Aborting."; exit 1; }

.PHONY: test
test: gen-files
	stack test --ta "--quickcheck-tests 200"

.PHONY: clean
clean:
	stack clean
	rm -f package.yaml .hlint.yaml .stylish-haskell.yaml
