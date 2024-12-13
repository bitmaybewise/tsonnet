(lang dune 3.16)

(name tsonnet)

(generate_opam_files true)

(source
 (gitlab bitmaybewise/tsonnet))

(authors "@bitmaybewise")

(maintainers "@bitmaybewise")

(license LICENSE)

(documentation https://gitlab.com/bitmaybewise/tsonnet)

(package
 (name tsonnet)
 (synopsis "A typed Jsonnet")
 (description "Like Jsonnet, but gradually typed")
 (depends
  (ocaml
   (>= 5.1.1))
  (dune
   (>= 3.16.0))
  (menhir
   (= 20240715)))
 (tags
  (jsonnet interpreter compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
