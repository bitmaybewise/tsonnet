(lang dune 3.16)

(name tsonnet)

(generate_opam_files true)

(source
 (gitlab bitmaybewise/tsonnet))

(authors "@bitmaybewise")

(maintainers "@bitmaybewise")

(license Apache-2.0)

(documentation https://gitlab.com/bitmaybewise/tsonnet)

(package
 (name tsonnet)
 (synopsis "A typed Jsonnet")
 (description "Like Jsonnet, but gradually typed")
 (depends
  (ocaml
   (>= 5.3.0))
  (dune
   (>= 3.16.0))
  (menhir
   (= 20240715))
  (alcotest
   (and
    :with-test
    (>= 1.8.0)
    (< 2.0.0)))
  (qcheck-core
   (and
    :with-test
    (= 0.25)))
  (qcheck-alcotest
   (and
    :with-test
    (= 0.25)))
  (ppxlib
   (and
    :with-test
    (= 0.36.0)))
  (ppx_deriving_qcheck
   (and :with-test))
  ; (bisect_ppx
  ;  (and
  ;   :with-test
  ;   (= 2.8.3)))
  (yojson
   (and
    (>= 2.2.2)
    (< 3.0.0))))
 (tags
  (jsonnet interpreter compiler config)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
