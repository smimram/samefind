(lang dune 2.9)
(name samefind)
(version 0.1.0)
(source (github smimram/samefind)) 
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
(license GPL-3.0-only)
(generate_opam_files)

(package
 (name samefind)
 (synopsis "Find files with same contents")
 (depends
   ocaml
 )
)
