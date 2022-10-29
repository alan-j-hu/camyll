module Otoml = Otoml_impl.T

open Jingoo

type name =
  | Index
  | Name of string

type page = {
  frontmatter : Otoml.t;
  content : string;
}

and item =
  | Bin of string
  | Dir of dir
  | Page of page

and dir = {
  dir_page : page option;
  children : (string, item) Hashtbl.t;
}

type taxonomy = {
  layout : string;
  items : (string, Jg_types.tvalue list) Hashtbl.t;
}

type t = {
  config : Config.t;
  langs : TmLanguage.t;
  taxonomies : (string, taxonomy) Hashtbl.t;
  tm_theme : Highlight.theme option;
  agda_links : (string list, string list) Hashtbl.t;
}

let rec jingoo_of_tomlvalue = function
  | Otoml.TomlBoolean b -> Jg_types.Tbool b
  | Otoml.TomlInteger i -> Jg_types.Tint i
  | Otoml.TomlFloat f -> Jg_types.Tfloat f
  | Otoml.TomlString s -> Jg_types.Tstr s
  | Otoml.TomlLocalDateTime d | Otoml.TomlOffsetDateTime d
  | Otoml.TomlLocalDate d | Otoml.TomlLocalTime d -> Jg_types.Tfloat d
  | Otoml.TomlArray a | Otoml.TomlTableArray a ->
    Jg_types.Tlist (List.map jingoo_of_tomlvalue a)
  | Otoml.TomlTable t | Otoml.TomlInlineTable t ->
    Jg_types.Tobj (List.map (fun (k, v) -> (k, jingoo_of_tomlvalue v)) t)

let jingoo_of_page url page =
  Jg_types.Tobj
    [ ("url", Jg_types.Tstr url)
    ; ("frontmatter", jingoo_of_tomlvalue page.frontmatter) ]

(* Add the Jingoo data to the taxonomy under the specified name. *)
let add_tag t taxonomy name data =
  match Hashtbl.find_opt t.taxonomies taxonomy with
  | None -> failwith ("Taxonomy " ^ taxonomy ^ " not defined")
  | Some taxonomy ->
    let name = Slug.slugify name in
    match Hashtbl.find_opt taxonomy.items name with
    | None -> Hashtbl.add taxonomy.items name [data]
    | Some items -> Hashtbl.replace taxonomy.items name (data :: items)

let add_taxonomies t url page =
  match Otoml.find_opt page.frontmatter Otoml.get_table ["taxonomies"] with
  | None -> ()
  | Some taxonomies ->
    taxonomies |> List.iter begin fun (taxonomy, v) ->
      match Otoml.get_array Otoml.get_string v with
      | exception Otoml.Type_error _ -> failwith "Expected an array of strings"
      | tags ->
        tags |> List.iter begin fun tag ->
          add_tag t taxonomy tag (jingoo_of_page url page)
        end
    end

(* Try to parse TOML frontmatter from the channel. *)
let parse_frontmatter chan =
  try
    let line = input_line chan in
    if line = "+++" then
      let buf = Buffer.create 100 in
      let rec loop () =
        let line = input_line chan in
        if line = "+++" then
          ()
        else (
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
        )
      in
      loop ();
      match Otoml.Parser.from_string_result (Buffer.contents buf) with
      | Ok toml -> toml
      | Error e -> failwith e
    else failwith "Missing ending frontmatter!"
  with End_of_file -> failwith "Missing frontmatter!"

let find_grammar t lang =
  match TmLanguage.find_by_name t.langs lang with
  | Some grammar -> Some grammar
  | None -> TmLanguage.find_by_filetype t.langs lang

(* Highlight the code blocks. *)
let highlight t theme =
  List.map begin function
    | Omd.Code_block(attr, "", code) ->
      Omd.Html_block(attr, Soup.pretty_print (Highlight.theme_block theme code))
    | Omd.Code_block(attr, lang, code) ->
      begin match find_grammar t lang with
        | None ->
          prerr_endline ("Warning: unknown language " ^ lang);
          let elt = Highlight.theme_block theme code in
          Omd.Html_block(attr, Soup.pretty_print elt)
        | Some grammar ->
          let elt = Highlight.highlight_block t.langs grammar theme code in
          Omd.Html_block(attr, Soup.pretty_print elt)
      end
    | x -> x
  end

let process_md t chan =
  match t.tm_theme with
  | Some theme ->
    Omd.to_html (highlight t theme (Omd.of_string (Filesystem.read_lines chan)))
  | None ->
    Omd.to_html (Omd.of_string (Filesystem.read_lines chan))

(* Intercepts [Failure _] exceptions to report the file name. *)
let with_in_smart f path =
  try Filesystem.with_in f path with
  | Failure e -> failwith (path ^ ": " ^ e)

let get_agda_module_name line =
  let open Angstrom in
  let whitespace = take_while1 (function
      | ' ' | '\n' | '\t' | '\r' -> true
      | _ -> false)
  in
  let name_part = take_while1 (function
      | '.' | ';' | '{' | '}' | '(' | ')' | '@' | '"'
      | ' ' | '\n' | '\t'| '\r' -> false
      | _ -> true)
  in
  let qualified_name = sep_by (char '.') name_part in
  let main =
    string "module" *> whitespace *> qualified_name
    <* whitespace <* string "where"
  in
  parse_string ~consume:Consume.Prefix main line

let source_dir dir =
  Filename.concat "content" (List.fold_left (Fun.flip Filename.concat) "" dir)

let dispatch t dir name =
  let read_path = Filename.concat (source_dir dir) name in
  match String.split_on_char '.' name with
  | ["index"; "html"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      (Index, Page { frontmatter; content = Filesystem.read_lines chan })
    end
  | ["index"; "md"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      (Index, Page { frontmatter; content = process_md t chan })
    end
  | [name; "lagda"; "md"] ->
    let module_name =
      read_path |> with_in_smart begin fun chan ->
        let rec loop () = match get_agda_module_name (input_line chan) with
          | Ok name -> name
          | Error _ -> loop ()
        in
        try loop () with End_of_file -> failwith "No module name found!"
      end
    in
    let exit_code =
      Sys.command
        (Filename.quote_command
           "agda"
           [ "--html"
           ; "--html-highlight=auto"
           ; "--html-dir=" ^ Config.agda_dest t.config
           ; read_path ])
    in
    if exit_code = 0 then (
      Hashtbl.replace t.agda_links module_name (name :: dir);
      let generated_md_name = Filename.concat
          (Config.agda_dest t.config)
          (String.concat "." module_name) ^ ".md"
      in
      let page = generated_md_name |> with_in_smart begin fun chan ->
          let frontmatter = parse_frontmatter chan in
          let content = process_md t chan in
          { frontmatter; content }
        end
      in
      Sys.remove generated_md_name;
      (Name name, Page page)
    ) else failwith ("Agda exited with code " ^ Int.to_string exit_code ^ "!")
  | [name; "html"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      (Name name, Page { frontmatter; content = process_md t chan })
    end
  | [name; "md"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      (Name name, Page { frontmatter; content = process_md t chan })
    end
  | _ ->
    read_path |> Filesystem.with_in_bin begin fun chan ->
      (Name name, Bin (Filesystem.read_bytes chan))
    end

let correct_agda_urls t node =
  let open Soup.Infix in
  node $$ "pre[class=\"Agda\"] > a[href]" |> Soup.iter begin fun node ->
    match Soup.attribute "href" node with
    | None -> failwith "Unreachable: href"
    | Some link ->
      let rec loop acc = function
        | [] -> failwith "Unreachable: Empty Agda link"
        | [ext] -> (acc, ext)
        | x :: xs -> loop (x :: acc) xs
      in
      let module_path, ext = loop [] (String.split_on_char '.' link) in
      match Hashtbl.find_opt t.agda_links (List.rev module_path) with
      | Some dir_path ->
        (* The link is to an internal module *)
        Soup.set_attribute
          "href"
          ("/" ^ String.concat "/" (List.rev dir_path) ^ "." ^ ext)
          node
      | None ->
        (* The link is to an external module *)
        let link = "/" ^ (Filename.concat t.config.Config.agda_dir link) in
        Soup.set_attribute "href" link node
  end

let render_from_file models url path =
  let env =
    { Jg_types.std_env with
      autoescape = false
    ; strict_mode = true
    ; template_dirs = ["includes"]
    ; filters =
        [ "format_date"
        , Jg_types.func_arg2_no_kw (fun format date ->
              let open CalendarLib in
              Jg_types.Tstr
                (Printer.Date.sprint
                   (Jg_types.unbox_string format)
                   (Date.from_unixfloat (Jg_types.unbox_float date))))
        ; "slugify"
        , Jg_types.func_arg1_no_kw (fun str ->
              Jg_types.Tstr (Slug.slugify (Jg_types.unbox_string str)))
        ]
    }
  in
  let path = Filename.concat "layouts" path in
  let print_err e = failwith (path ^ ": " ^ url ^ ":" ^ e) in
  try Jg_template.from_file ~env ~models path with
  | Failure e -> failwith (print_err e)
  | Invalid_argument e -> failwith (print_err e)
  | Jingoo.Jg_types.SyntaxError e -> failwith (print_err e)

let render_page pages url page =
  match Otoml.find_opt page.frontmatter Otoml.get_string ["layout"] with
  | None -> page.content
  | Some path ->
    let models =
      [ "content", Jg_types.Tstr page.content
      ; "pages", Jg_types.Tlist pages
      ; "frontmatter", jingoo_of_tomlvalue page.frontmatter ]
    in
    render_from_file models url path

let relativize_urls url node =
  let open Soup.Infix in
  let replace attr =
    node $$ ("[" ^ attr ^ "]") |> Soup.iter begin fun node ->
      match Soup.attribute attr node with
      | None -> failwith ("Unreachable: attribute " ^ attr ^ " not found!")
      | Some link ->
        Soup.set_attribute attr (Url.relativize ~src:url ~dest:link) node
    end
  in
  replace "href";
  replace "src"

let compile_page t siblings path url page =
  let content = render_page siblings url page in
  let output = Soup.parse content in
  add_taxonomies t url page;
  correct_agda_urls t output;
  relativize_urls url output;
  path |> Filesystem.with_out begin fun out_chan ->
    output_string out_chan (Soup.pretty_print output)
  end

let rec load_dir t dir =
  let read_dir = source_dir dir in
  let files = Sys.readdir read_dir in
  let pages = Hashtbl.create (Array.length files) in
  let index = Array.fold_left (fun index name ->
      let path = Filename.concat read_dir name in
      if List.exists (Fun.flip Re.execp path) t.config.Config.exclude then
        index
      else if Sys.is_directory path then (
        let dir = load_dir t (name :: dir) in
        if Hashtbl.mem pages name then
          failwith ("Duplicate page " ^ path ^ "!")
        else (
          Hashtbl.add pages name (Dir dir);
          index
        )
      ) else
        let name, data = dispatch t dir name in
        match index, name with
        | _, Name name ->
          if Hashtbl.mem pages name then
            failwith ("Duplicate page " ^ path ^ "!")
          else (
            Hashtbl.add pages name data;
            index
          )
        | Some _, Index -> failwith ("Duplicate index " ^ path ^ "!")
        | None, Index ->
          match data with
          | Bin _ -> failwith ("Index is a binary: " ^ path ^ "!")
          | Dir _ -> failwith ("Index is a directory: " ^ path ^ "!")
          | Page page -> Some page
    ) None files
  in
  { dir_page = index; children = pages }

let rec compile_dir t root url_prefix { dir_page; children } =
  let pages =
    Hashtbl.to_seq children
    |> Seq.filter_map (function
        | name, Page page ->
          Some (jingoo_of_page (url_prefix ^ name ^ ".html") page)
        | _, _ -> None)
    |> List.of_seq
  in
  Filesystem.touch_dir root;
  children |> Hashtbl.iter begin fun name item ->
    match item with
    | Bin data ->
      let dest = Filename.concat root name in
      Filesystem.with_out_bin (Fun.flip output_string data) dest
    | Dir subdir ->
      compile_dir t (Filename.concat root name) (url_prefix ^ name ^ "/") subdir
    | Page page ->
      let dest = Filename.concat root name ^ ".html" in
      compile_page t pages dest (url_prefix ^ name ^ ".html") page
  end;
  match dir_page with
  | None -> ()
  | Some page ->
    compile_page
      t pages (Filename.concat root "index.html") url_prefix page

let build_taxonomy t name taxonomy =
  let dir = Filename.concat t.config.Config.dest_dir name in
  Filesystem.touch_dir dir;
  taxonomy.items |> Hashtbl.iter begin fun slugified_tag pages ->
    let output_path = Filename.concat dir slugified_tag  ^ ".html" in
    let content =
      render_from_file
        [ "pages", Jg_types.Tlist pages
        ; "name", Jg_types.Tstr slugified_tag ]
        output_path
        taxonomy.layout
    in
    let output = Soup.parse content in
    correct_agda_urls t output;
    let url = "/" ^ name ^ "/" ^ slugified_tag ^ ".html" in
    relativize_urls url output;
    output_path |> Filesystem.with_out begin fun out_chan ->
      output_string out_chan (Soup.pretty_print output)
    end
  end

let build_taxonomies t =
  Hashtbl.iter (build_taxonomy t) t.taxonomies

let build_with_config config =
  Filesystem.touch_dir config.Config.dest_dir;
  let langs = TmLanguage.create () in
  begin match Sys.readdir "grammars" with
    | exception (Sys_error _) -> ()
    | names ->
      names |> Array.iter begin fun name ->
        if Sys.is_directory (Filename.concat "grammars" name) then
          ()
        else
          let path = Filename.concat "grammars" name in
          try
            let lang =
              path |> with_in_smart begin fun chan ->
                match Filename.extension name with
                | ".plist" | ".tmLanguage" ->
                  chan
                  |> Markup.channel
                  |> Plist_xml.parse_exn
                  |> TmLanguage.of_plist_exn
                | ".json" ->
                  chan
                  |> Ezjsonm.from_channel
                  |> TmLanguage.of_ezjsonm_exn
                | ".yaml" | ".YAML-tmLanguage" ->
                  chan
                  |> Filesystem.read_bytes
                  |> Yaml.of_string_exn
                  |> TmLanguage.of_ezjsonm_exn
                | ext ->
                  failwith
                    ("Unsupported syntax extension " ^ ext
                     ^ ": .plist / .tmLanguage, .json, "
                     ^ "and .yaml / .YAML-tmLanguage are supported")
              end
            in
            TmLanguage.add_grammar langs lang
          with
          | Oniguruma.Error s -> failwith (path ^ ": Oniguruma: " ^ s)
          | Plist_xml.Parse_error s -> failwith (path ^ ": " ^ s)
          | Ezjsonm.Parse_error(_, s) -> failwith (path ^ ": " ^ s)
          | Invalid_argument s -> failwith (path ^ ": " ^ s)
          | TmLanguage.Error s -> failwith (path ^ ": " ^ s)
      end
  end;
  let tm_theme =
    if Sys.file_exists "theme.tmTheme" then
      Some ("theme.tmTheme" |> with_in_smart begin fun chan ->
          chan
          |> Markup.channel
          |> Plist_xml.parse_exn
          |> Highlight.theme_of_plist
        end)
    else
      None
  in
  let t =
    { config
    ; langs
    ; taxonomies = Hashtbl.create 2
    ; tm_theme
    ; agda_links = Hashtbl.create 29 }
  in
  Filesystem.remove_dir t.config.Config.dest_dir;
  config.Config.taxonomies |> List.iter begin fun taxonomy ->
    Hashtbl.add t.taxonomies taxonomy.Config.name
      { layout = taxonomy.Config.layout
      ; items = Hashtbl.create 11 }
  end;
  let dir = load_dir t [] in
  compile_dir t t.config.Config.dest_dir "/" dir;
  build_taxonomies t

let build () = Config.with_config build_with_config
