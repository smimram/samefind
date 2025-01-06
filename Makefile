all:
	@dune build

test:
	@dune test --force

.PHONY: test
