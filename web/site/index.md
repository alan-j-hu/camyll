+++
title = "Camyll"
layout = "main.jingoo"
+++
```
$ opam install camyll
```

## Markdown

Camyll converts files ending in `.md` from Markdown to HTML.

## Literate Agda

Camyll recognizes files ending in `.lagda.md` as Literate Agda files and
invokes the Agda compiler to preprocess the Agda code blocks.

## Syntax Highlighting

Instead of supporting a fixed set of languages, Camyll lets the user provide
TextMate grammars in the site directory for any desired language. Camyll uses
TextMate themes to assign colors to tokens.

## Tags and Taxonomies

Camyll supports tags and user-defined taxonomies.
