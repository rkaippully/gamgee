.PHONY: build
build:
	stack build
	hlint .

.PHONY: test
test:
	stack test --ta "--quickcheck-tests 200"

.PHONY: clean
clean:
	stack clean
