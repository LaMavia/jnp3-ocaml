(lang dune 3.10)

(name jnp3_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name jnp3_ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (tags
  (topics "to describe" your project))
 (depends 
  (dune (>= 3.10))
  (ocaml (= 4.13.1))
  (ocaml-lsp-server (= 1.9.1))
  ocamlformat
 ))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
