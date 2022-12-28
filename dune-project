(lang dune 2.8)

(generate_opam_files true)
(cram enable)

(authors "Nicolas “Niols” Jeannerod")
(maintainers "Nicolas “Niols” Jeannerod <niols@niols.fr>")

(source (github Niols/aoc2022))

(package
 (name aoc2022)
 (synopsis "My contribution to the Advent of Code in 2022")
 (depends
  logs
  next
  (ppx_inline_test (or :build :with-test))
  (ppx_deriving    :build)))
