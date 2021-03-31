(lang dune 2.8.5)
(name bactrian)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github /bactrian))
(maintainers "Benjamin Tucker <betucker@ucsd.edu>")
(authors "Benjamin Tucker <betucker@ucsd.edu>")

(package
 (name bactrian)
 (synopsis "A really stupid pairwise test generator")
 (description "\
A really stupid pairwise test generator
")
 (documentation https://.github.io/bactrian/)
 (depends alcotest fmt logs))
