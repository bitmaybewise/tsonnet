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
