(lang dune 3.17)

(name smvlib)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Mark Wutka <mark.wutka@vanderbilt.edu>")

(maintainers "Mark Wutka <mark.wutka@vanderbilt.edu>")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name smvlib)
 (synopsis "SMV AST, parser, and printer")
 (description "A library for parsing, printing, and manipulating SMV files")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
