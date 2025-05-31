FROM ocaml/opam:alpine-ocaml-5.3

RUN opam install dune
ADD tsonnet.opam .
RUN opam install --deps-only --with-test .
