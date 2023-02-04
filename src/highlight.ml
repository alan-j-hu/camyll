type scope = string list
type scope_stack = scope list
type selector = { select : scope_stack }

type token = {
  background : string option;
  foreground : string option;
  is_bold : bool;
  is_italics : bool;
  is_underline : bool;
  selectors : selector list;
}

type theme = {
  background : string option;
  foreground : string option;
  tokens : token list;
}

let find_exn key obj =
  match List.assoc_opt key obj with
  | Some v -> v
  | None -> failwith (key ^ " not found.")

let get_dict = function
  | `Dict d -> d
  | _ -> failwith "Type error: Expected dict."

let get_list f = function
  | `Array l -> List.map f l
  | _ -> failwith "Type error: Expected list."

let get_string = function
  | `String s -> s
  | _ -> failwith "Type error: Expected string."

(* Checks if a string represents a valid CSS color. *)
let validate_color str =
  let open Angstrom in
  let is_hexadecimal = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  let rgb_maybe_a =
    char '#'
    *> (count 3 (satisfy is_hexadecimal) *> end_of_input
       <|> count 4 (satisfy is_hexadecimal) *> end_of_input
       <|> count 6 (satisfy is_hexadecimal) *> end_of_input
       <|> count 8 (satisfy is_hexadecimal) *> end_of_input)
  in
  parse_string ~consume:All rgb_maybe_a str

let get_styles str =
  let tokens = str |> String.split_on_char ' ' |> List.filter (( <> ) "") in
  let rec loop ~is_bold ~is_italic ~is_underline = function
    | [] -> (is_bold, is_italic, is_underline)
    | "bold" :: tokens -> loop ~is_bold:true ~is_italic ~is_underline tokens
    | "italic" :: tokens -> loop ~is_bold ~is_italic:true ~is_underline tokens
    | "underline" :: tokens ->
      loop ~is_bold ~is_italic ~is_underline:true tokens
    | token :: _ -> raise (Invalid_argument ("Unknown style " ^ token ^ "!"))
  in
  loop ~is_bold:false ~is_italic:false ~is_underline:false tokens

let validate_color_exn c =
  let str = get_string c in
  match validate_color str with
  | Ok () -> str
  | Error e -> raise (Invalid_argument ("Invalid color: " ^ str ^ " " ^ e))

let token_of_plist (plist : Plist_xml.t) : token option =
  (* TODO: Handle selector substraction operator *)
  let make select = { select } in
  let d = get_dict plist in
  match List.assoc_opt "scope" d with
  | None -> None
  | Some scope ->
    let selectors =
      scope |> get_string |> String.split_on_char ','
      |> List.map (fun str ->
             str |> String.split_on_char ' ' |> List.map String.trim
             |> List.filter (( <> ) "")
             |> List.rev
             |> List.map (String.split_on_char '.')
             |> make)
    in
    let settings = find_exn "settings" d |> get_dict in
    let is_bold, is_italics, is_underline =
      match List.assoc_opt "fontStyle" settings with
      | None -> (false, false, false)
      | Some styles -> get_styles (get_string styles)
    in
    Some
      {
        background =
          Option.map validate_color_exn (List.assoc_opt "background" settings);
        foreground =
          Option.map validate_color_exn (List.assoc_opt "foreground" settings);
        is_bold;
        is_italics;
        is_underline;
        selectors;
      }

let theme_of_plist plist =
  let d = get_dict plist in
  let tokens = find_exn "settings" d in
  let tokens = get_list Fun.id tokens in
  match tokens with
  | [] -> failwith "Empty ruleset!"
  | main :: tokens ->
    let settings = main |> get_dict |> find_exn "settings" |> get_dict in
    {
      tokens = List.filter_map token_of_plist tokens;
      background =
        Option.map validate_color_exn (List.assoc_opt "background" settings);
      foreground =
        Option.map validate_color_exn (List.assoc_opt "foreground" settings);
    }

let prefix_length scope selector =
  let rec loop acc scope selector =
    match (scope, selector) with
    | [], _ :: _ -> None (* Selector is more specific than the scope *)
    | [], [] -> Some acc
    | x :: xs, y :: ys when x = y -> loop (acc + 1) xs ys
    | _ :: _, _ -> Some acc
  in
  loop 0 scope selector

let rec score_selector scopes_stack (sels : scope_stack) =
  (* TextMate's scoring system is arcane and the documentation does not give
     a complete specification:

     - https://macromates.com/manual/en/scope_selectors
     - http://textmate.1073791.n5.nabble.com/formal-definition-of-scope-selector-syntax-td12109.html
     - https://macromates.com/blog/2005/introduction-to-scopes/

     This specification just adds up all the depths. *)
  match (scopes_stack, sels) with
  | [], _ :: _ -> None
  | scopes :: scopes_stack, sel :: sels ->
    Option.bind (prefix_length scopes sel) (fun len ->
        Option.map (( + ) len) (score_selector scopes_stack sels))
  | _, [] -> Some 0

let score_token scopes_stack (token : token) =
  let f acc next =
    match (acc, score_selector scopes_stack next.select) with
    | None, None -> None
    | None, Some score -> Some score
    | Some _, None -> acc
    | Some score1, Some score2 ->
      if score1 > score2 then Some score1 else Some score2
  in
  List.fold_left f None token.selectors

let style_of_token (token : token) =
  let color =
    match token.foreground with
    | None -> ""
    | Some foreground -> "color: " ^ foreground ^ ";"
  in
  let background =
    match token.background with
    | None -> ""
    | Some background -> "background: " ^ background ^ ";"
  in
  let style = if token.is_italics then "font-style: italic;" else "" in
  let weight = if token.is_bold then "font-weight: bold;" else "" in
  let decoration =
    if token.is_underline then "text-decoration: underline;" else ""
  in
  color ^ background ^ style ^ weight ^ decoration

let create_signals theme scopes i j line : Markup.signal list =
  assert (j > i);
  let inner_text = String.sub line i (j - i) in
  let scopes = List.map (String.split_on_char '.') scopes in
  let token =
    List.fold_left
      (fun acc next ->
        let new_score = score_token scopes next in
        match acc with
        | None -> (
          match new_score with
          | None -> None
          | Some _ -> Some next)
        | Some old -> (
          let old_score = score_token scopes old in
          match (old_score, new_score) with
          | None, None -> None
          | None, Some _ -> Some next
          | Some _, None -> Some old
          | Some score1, Some score2 ->
            if score1 < score2 then Some next else Some old))
      None theme.tokens
  in
  match token with
  | Some token ->
    [
      `Start_element (("", "span"), [ (("", "style"), style_of_token token) ]);
      `Text [ inner_text ];
      `End_element;
    ]
  | None -> [ `Text [ inner_text ] ]

let rec highlight_tokens theme i rev line = function
  | [] -> rev
  | tok :: toks ->
    let j = TmLanguage.ending tok in
    let signals = create_signals theme (TmLanguage.scopes tok) i j line in
    highlight_tokens theme j (List.rev_append signals rev) line toks

let rec highlight_lines langs grammar theme stack rev lines =
  match lines with
  | [] -> rev
  | line :: lines ->
    let tokens, stack = TmLanguage.tokenize_exn langs grammar stack line in
    let rev = highlight_tokens theme 0 (`End_element :: rev) line tokens in
    let rev =
      `Start_element (("", "span"), [ (("", "class"), "sourceLine") ]) :: rev
    in
    highlight_lines langs grammar theme stack rev lines

(* Splits a string into lines, keeping the newline at the end. Assumes that
   the string ends with a newline. *)
let lines s =
  let rec loop lines i =
    match String.index_from_opt s i '\n' with
    | None -> List.rev lines
    | Some j -> loop (String.sub s i (j - i + 1) :: lines) (j + 1)
  in
  loop [] 0

(* Applies a theme to a list of spans. *)
let theme_spans theme =
  let color =
    match theme.foreground with
    | None -> ""
    | Some color -> "color: " ^ color ^ ";"
  in
  let background =
    match theme.background with
    | None -> ""
    | Some background -> "background: " ^ background ^ ";"
  in
  let style = color ^ background in
  [
    `Start_element (("", "code"), []);
    `Start_element (("", "pre"), [ (("", "style"), style) ]);
  ]

(* Highlights a block of code. *)
let highlight_block langs grammar theme code =
  let lines = lines code in
  let spans =
    try
      highlight_lines langs grammar theme TmLanguage.empty (theme_spans theme)
        lines
    with
    | Oniguruma.Error s -> failwith s
    | TmLanguage.Error s -> failwith s
  in
  List.rev (`End_element :: `End_element :: spans)

(* Themes a block of code without tokenizing anything. *)
let theme_block theme code =
  List.rev (`End_element :: `End_element :: `Text [ code ] :: theme_spans theme)
