type exists_node = Node : 'a Soup.node -> exists_node

(* This is the same mapping that is used in Pygments. See the code in
   https://github.com/pygments/pygments/blob/6ded9db39463372e5205a36bea72d6de516ece69/pygments/token.py#L124
   and
   https://github.com/alecthomas/chroma/blob/9dc3c8c529257cd96c927ca7d2f35d83264c22e8/types.go#L228. *)
let get_class = function
  | "whitespace" :: _ -> Some "w"
  | "escape" :: _ -> Some "esc"
  | "error" :: _ -> Some "err"
  | "other" :: _ -> Some "x"
  | "keyword" :: "constant" :: _ -> Some "kc"
  | "keyword" :: "declaration" :: _ -> Some "kd"
  | "keyword" :: "namespace" :: _ -> Some "kn"
  | "keyword" :: "pseudo" :: _ -> Some "kp"
  | "keyword" :: "reserved" :: _ -> Some "kr"
  | "keyword" :: "type" :: _ -> Some "kt"
  | "keyword" :: _ -> Some "k"
  | "name" :: "attribute" :: _ -> Some "na"
  | "name" :: "builtin" :: "pseudo" :: _ -> Some "bp"
  | "name" :: "builtin" :: _ -> Some "nb"
  | "name" :: "class" :: _ -> Some "nc"
  | "name" :: "constant" :: _ -> Some "no"
  | "name" :: "decorator" :: _ ->  Some "nd"
  | "name" :: "entity" :: _ -> Some "ni"
  | "name" :: "exception" :: _ -> Some "ne"
  | "name" :: "function" :: "magic" :: _ -> Some "fm"
  | "name" :: "function" :: _ -> Some "nf"
  | "name" :: "property" :: _ -> Some "py"
  | "name" :: "label" :: _ -> Some "nl"
  | "name" :: "namespace" :: _ -> Some "nn"
  | "name" :: "other" :: _ -> Some "nx"
  | "name" :: "tag" :: _ -> Some "nt"
  | "name" :: "variable" :: "class" :: _ ->  Some "vc"
  | "name" :: "variable.global" :: _ ->  Some "vg"
  | "name" :: "variable.instance" :: _ -> Some "vi"
  | "name" :: "variable" :: "magic" :: _ -> Some "vm"
  | "name" :: "variable" :: _ ->  Some "nv"
  | "name" :: _ -> Some "n"
  | "literal" :: "date" :: _ ->  Some "ld"
  | "string" :: "affix" :: _ -> Some "sa"
  | "literal" :: _ -> Some "l"
  | "string" :: "backtick" :: _ -> Some "sb"
  | "string" :: "char" :: _ -> Some "sc"
  | "string" :: "delimiter" :: _ -> Some "dl"
  | "string" :: "doc" :: _ -> Some "sd"
  | "string" :: "double" :: _ -> Some "s2"
  | "string" :: "escape" :: _ -> Some "se"
  | "string" :: "heredoc" :: _ -> Some "sh"
  | "string" :: "interpol":: _ -> Some "si"
  | "string" :: "other" :: _ ->  Some "sx"
  | "string" :: "regex" :: _ ->  Some "sr"
  | "string" :: "single" :: _ -> Some "s1"
  | "string" :: "symbol" :: _ -> Some "ss"
  | "string" :: _ -> Some "s"
  | "number" :: "bin" :: _ -> Some "mb"
  | "number" :: "float" :: _ -> Some "mf"
  | "number" :: "hex" :: _ -> Some "mh"
  | "number" :: "integer" :: "long" :: _ -> Some "il"
  | "number" :: "integer" :: _ -> Some "mi"
  | "number" :: "oct" :: _ -> Some "mo"
  | "number" :: _ -> Some "m"
  | "operator" :: "word" :: _ -> Some "ow"
  | "operator" :: _ -> Some "o"
  (* Workaround to highlight comments and strings correctly *)
  | "punctuation" :: "definition" :: "comment" :: _ -> Some "c"
  | "punctuation" :: "definition" :: "string" :: _ -> Some "s"
  | "punctuation" :: _ -> Some "p"
  | "comment" :: "hashbang" :: _ -> Some "ch"
  | "comment" :: "multiline":: _ -> Some "cm"
  | "comment" :: "preproc":: _ -> Some "cp"
  | "comment" :: "preprocfile":: _ -> Some "cpf"
  | "comment" :: "single" :: _ -> Some "c1"
  | "comment" :: _ -> Some "c"
  | "gomment" :: "special" :: _ -> Some "cs"
  | "generic" :: "deleted" :: _ -> Some "gd"
  | "generic" :: "emph" :: _ -> Some "ge"
  | "generic" :: "error" :: _ -> Some "gr"
  | "generic" :: "heading" :: _ -> Some "gh"
  |" generic" :: "inserted" :: _ -> Some "gi"
  | "generic" :: "output" :: _ -> Some "go"
  | "generic" :: "prompt" :: _ -> Some "gp"
  | "generic" :: "strong" :: _ -> Some "gs"
  | "generic" :: "subheading" :: _ -> Some "gu"
  | "generic" :: "traceback" :: _ -> Some "gt"
  | "generic" :: _ -> Some "g"
  | _ -> None

let create_node scopes i j line =
  assert (j > i);
  let inner_text = String.sub line i (j - i) in
  let class_ = match scopes with
    | [] -> None
    | scope :: _ ->
      get_class (String.split_on_char '.' (String.lowercase_ascii scope))
  in
  match class_ with
  | Some class_ -> Node (Soup.create_element ~class_ "span" ~inner_text)
  | None -> Node (Soup.create_text inner_text)

let rec highlight_tokens i acc line = function
  | [] -> List.rev acc
  | tok :: toks ->
    let j = TmLanguage.ending tok in
    let span = create_node (TmLanguage.scopes tok) i j line in
    highlight_tokens j (span :: acc) line toks

let highlight_line langs grammar stack line =
  let tokens, stack = TmLanguage.tokenize_exn langs grammar stack line in
  let nodes = highlight_tokens 0 [] line tokens in
  let a = Soup.create_element "a" ~class_:"sourceLine" in
  List.iter (fun (Node node) -> Soup.append_child a node) nodes;
  a, stack

(** Maps over the list while keeping track of some state.
    Discards the state because I don't need it. *)
let rec map_fold f acc = function
  | [] -> []
  | x :: xs ->
    let y, acc = f acc x in
    y :: map_fold f acc xs

let highlight_block langs grammar code =
  let lines = String.split_on_char '\n' code in
  (* Some patterns don't work if there isn't a newline *)
  let lines = List.map (fun s -> s ^ "\n") lines in
  let a's = map_fold (highlight_line langs grammar) TmLanguage.empty lines in
  let code = Soup.create_element "code" ~class_:"highlight" in
  List.iter (Soup.append_child code) a's;
  let pre = Soup.create_element "pre" in
  Soup.append_child pre code;
  pre
