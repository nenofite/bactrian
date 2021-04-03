(lang dune 2.8.5)

(name bactrian)

(implicit_transitive_deps true)

(generate_opam_files true)

(source
 (github nenofite/bactrian))

(maintainers "Benjamin Tucker <benjamin.tucker@hey.com>")

(authors "Benjamin Tucker <benjamin.tucker@hey.com>")

(package
 (name bactrian)
 (synopsis "Pairwise test helper")
 (description
  "A stupid-simple tool that checks if your test suite contains all possible pairs, and suggests tests to add if not.\n")
 (depends alcotest fmt logs core_kernel))
