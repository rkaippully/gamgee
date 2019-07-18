dhall-files := $(shell find ./dhall -type f)
dhall-derived-files = package.yaml .hlint.yaml .stylish-haskell.yaml

.PHONY: build
build: $(dhall-derived-files)
	stack build
	hlint .

package.yaml: $(dhall-files)
	echo "./dhall/package.dhall" | dhall-to-yaml > $@

.hlint.yaml: $(dhall-files)
	echo "./dhall/hlint.dhall" | dhall-to-yaml --omitNull > $@

.stylish-haskell.yaml: $(dhall-files)
	echo "./dhall/stylish-haskell.dhall" | dhall-to-yaml > $@

.PHONY: test
test:
	stack test --ta "--quickcheck-tests 200"

.PHONY: clean
clean:
	stack clean
	rm -f package.yaml .hlint.yaml
