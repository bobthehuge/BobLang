(lang dune 3.6)

(name boblang)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "BobTheHuge")

(maintainers "BobTheHuge")

(using menhir 2.0)

(package
 (name boblang)
 (synopsis "A quick language creation test")
 (description "Nothing else to add...")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
