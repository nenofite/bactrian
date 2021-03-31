(lang dune 2.8.5)

(name bactrian)

(implicit_transitive_deps false)

(generate_opam_files true)

(source
 (github nenofite/bactrian))

(maintainers "Benjamin Tucker <benjamin.tucker@hey.com>")

(authors "Benjamin Tucker <benjamin.tucker@hey.com>")

(package
 (name bactrian)
 (synopsis "A really stupid pairwise test generator")
 (description "A really stupid pairwise test generator\n")
 (depends alcotest fmt logs))
