default: test

.PHONY: test
test:
	dune runtest

.PHONY: coverage
coverage:
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html

.PHONY: clean
clean:
	dune clean
	rm -rf _coverage/

.PHONY: prepare-dev-setup
prepare-dev-setup:
	mise install
	opam install dune utop ocamlformat ocaml-lsp-server
	opam install --deps-only --with-test .

.PHONY: explain
explain:
	menhir --explain lib/parser.mly
