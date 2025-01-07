all:
	@dune build

test:
	@dune test --force

install:
	@dune install

.PHONY: test
