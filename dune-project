(lang dune 3.8)

(using melange 0.1)

(generate_opam_files true)

(implicit_transitive_deps false)

(name astexplorer-refmt)

(source
 (github jchavarri/astexplorer-refmt))

(license ISC)

(maintainers
 "Javier Chávarri <javier.chavarri@gmail.com>"
 "Pedro Lisboa <pedrobslisboa@gmail.com>")

(authors "Javier Chávarri <javier.chavarri@gmail.com>")

(package
 (name astexplorer-refmt)
 (synopsis "OCaml AST explorer for Reason and OCaml")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.0.0))
  (reason
   (>= 3.17.2))
  (js_of_ocaml-ppx
   (>= 6.2.0))
  ; Library dependencies
  (js_of_ocaml
   (>= 6.2.0))
  (yojson
   (>= 3.0.0))
  (ppxlib
   (= 0.37.0))
  ; Dev dependencies
  (ocamlformat
   (and
    (= 0.26.2)
    :with-dev-setup))
  ; We use ocamlformat on the tests
  (ocaml-lsp-server :with-dev-setup)))
