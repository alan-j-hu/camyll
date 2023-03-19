(lang dune 2.7)
(name camyll)
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

Features:

- Conversion from Markdown to HTML
- Syntax highlighting of any language via user-provided TextMate grammars
- Post tagging
- Processing of literate Agda")
 (tags (blog web website))
 (depends
  (angstrom (>= 0.15))
  (calendar (>= 2.01))
  (cmdliner (>= 1.1))
  (ezjsonm (>= 1.3))
  (httpaf (>= 0.7.1))
  (httpaf-lwt-unix (>= 0.7.1))
  (jingoo (>= 1.4))
  (markup (>= 0.8))
  (ocaml (>= 4.14))
  (omd (>= 2.0.0~alpha2))
  (otoml (>= 0.9.3))
  (plist-xml (>= 0.5))
  (re (>= 1.9))
  (slug (>= 1.0))
  (textmate-language (>= 0.3.2))
  (uri (>= 4.2))
  (yaml (>= 3.1))))
