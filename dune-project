(lang dune 3.16)

(name advent_of_code)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name advent_of_code)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune core zarith (ocamlformat (= "0.27.0")) ocaml-lsp-server cohttp cohttp-async core_unix async async_ssl)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
