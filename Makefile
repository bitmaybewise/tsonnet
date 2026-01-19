COVERAGE_DIR = $(shell pwd)/_coverage/

default: test

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: coverage
coverage:
	dune clean
	rm -rf $(COVERAGE_DIR)
	mkdir $(COVERAGE_DIR)
	BISECT_FILE=$(COVERAGE_DIR) dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(COVERAGE_DIR)
	bisect-ppx-report cobertura $(COVERAGE_DIR)/cobertura.xml --coverage-path $(COVERAGE_DIR)
	bisect-ppx-report summary --coverage-path $(COVERAGE_DIR)

.PHONY: prepare-dev-setup
prepare-dev-setup:
	mise install
	opam init
	opam install dune utop ocamlformat ocaml-lsp-server
	opam install --deps-only --with-test .

.PHONY: explain
explain:
	menhir --explain lib/parser.mly
