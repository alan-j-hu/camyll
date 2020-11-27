(lang dune 2.7)
(name camyll)
(version 0.2.0)
(generate_opam_files true)

(license MIT)
(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(homepage "https://alan-j-hu.github.io/camyll")
(source (github alan-j-hu/camyll))

(package
 (name camyll)
 (synopsis "A static site generator")
 (description "Camyll is a static site generator.

- Supports syntax highlighting of arbitrary languages via grammar files
  included in the site directory
- Aware of Literate Agda")
 (tags (blog))
 (depends
  (calendar (and (>= 2.0) (< 3.0)))
  (cmdliner (and (>= 1.0) (< 2.0)))
  (ezjsonm (and (>= 1.1) (< 2.0)))
  (jingoo (and (>= 1.4) (< 2.0)))
  (lambdasoup (and (>= 0.7) (< 0.8)))
  (markup (and (>= 0.8) (< 2.0)))
  (ocaml (>= 4.08))
  (omd (and (>= 1.3.1) (< 2.0)))
  (textmate-language (and (>= 0.2) (< 0.3)))
  (plist-xml (< 0.4))
  (re (and (>= 1.9) (< 2.0)))
  (yaml (and (>= 2.1) (< 3.0)))))