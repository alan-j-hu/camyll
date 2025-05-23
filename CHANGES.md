## 0.4.4 (April 6, 2025)

- Switch Markdown engine from OMD to Cmarkit.

## 0.4.3 (March 14, 2023)

- Upgrade `plist-xml` to version 0.5.

## 0.4.2 (February 4, 2023)

- Remove dependency on `lambdasoup`, allowing Camyll to build on OCaml 5
- Use `In_channel` and `Out_channel` modules, requiring OCaml 4.14 or higher

## 0.4.1 (October 29, 2022)

- Upgrade `calendar`, `cmdliner`, and `textmate-language` dependencies and
  replace usage of deprecated `cmdliner` API.
- Catch stray exceptions.
- Support JSON and YAML grammar files.

## 0.4.0 (November 28, 2021)

- Switch from To.ml to OTOML library (#1, Daniil Baturin).
- Close a leaked file handle.
- Use `slug` library instead of handrolled function for generating slugs.

## 0.3.0 (July 28, 2020)

- Validate colors from TextMate themes.
- Implement other theme attributes.
- Use proper URI parsing library in server; previous code didn't handle ? and
  #.
- Use opinionated directory names instead of making them configurable.
- Determine Agda module names by parsing file instead of deriving it from the
  filepath. This makes it possible to set the project root in a different
  directory (such as through `.agda-lib`).
- Don't leave Agda-processed Markdown files in the Agda documentation directory.

## 0.2.0 (July 21, 2020)

- Switch template language from Mustache to Jingoo.
- Switch config language from YAML to TOML.
- Require frontmatter.
- Add the `serve` command for serving a site.
- Add taxonomies.
- Use TextMate themes for syntax highlighting.

## 0.1.0 (October 15, 2020)

Initial release.
