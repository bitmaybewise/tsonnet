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
   (= 20240715))
  (alcotest
   (and
    :with-test
    (>= 1.8.0)))
  (bisect_ppx
   (and
    :with-test
    (>= 2.8.3)))
  (yojson
   (>= 2.2.2)))
 (tags
  (jsonnet interpreter compiler)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
