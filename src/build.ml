open Jingoo

type 'a doc = {
  name : string;
  subdir : string;
  frontmatter : Toml.Types.table;
  content : 'a;
}

type taxonomy = {
  template : string;
  items : (string, Jg_types.tvalue list) Hashtbl.t;
}

type t = {
  config : Config.t;
  langs : TmLanguage.t;
  taxonomies : (string, taxonomy) Hashtbl.t;
}

let rec jingoo_of_tomlvalue = function
  | Toml.Types.TBool b -> Jg_types.Tbool b
  | Toml.Types.TInt i -> Jg_types.Tint i
  | Toml.Types.TFloat f -> Jg_types.Tfloat f
  | Toml.Types.TString s -> Jg_types.Tstr s
  | Toml.Types.TDate d -> Jg_types.Tfloat d
  | Toml.Types.TArray a -> jingoo_of_tomlarray a
  | Toml.Types.TTable t -> jingoo_of_tomltable t

and jingoo_of_tomlarray = function
  | Toml.Types.NodeEmpty -> Jg_types.Tlist []
  | Toml.Types.NodeBool bs -> Jg_types.Tlist (List.map Jg_types.box_bool bs)
  | Toml.Types.NodeInt is -> Jg_types.Tlist (List.map Jg_types.box_int is)
  | Toml.Types.NodeFloat fs -> Jg_types.Tlist (List.map Jg_types.box_float fs)
  | Toml.Types.NodeString ss -> Jg_types.Tlist (List.map Jg_types.box_string ss)
  | Toml.Types.NodeDate ds -> Jg_types.Tlist (List.map Jg_types.box_float ds)
  | Toml.Types.NodeArray ars ->
    Jg_types.Tlist (List.map jingoo_of_tomlarray ars)
  | Toml.Types.NodeTable tbls ->
    Jg_types.Tlist (List.map jingoo_of_tomltable tbls)

and jingoo_of_tomltable table =
  Jg_types.Tobj (Toml.Types.Table.fold (fun k v acc ->
      (Toml.Types.Table.Key.to_string k, jingoo_of_tomlvalue v) :: acc
    ) table [])

let jingoo_of_page page =
  Jg_types.Tobj
    [ ("url", Jg_types.Tstr ("/" ^ page.subdir ^ "/" ^ page.name))
    ; ("frontmatter", jingoo_of_tomltable page.frontmatter) ]

(* Add the Jingoo data to the taxonomy under the specified name. *)
let add_taxonomy t taxonomy name data =
  match Hashtbl.find_opt t.taxonomies taxonomy with
  | None -> failwith ("Taxonomy " ^ taxonomy ^ " not defined")
  | Some taxonomy ->
    match Hashtbl.find_opt taxonomy.items name with
    | None -> Hashtbl.add taxonomy.items name [data]
    | Some items -> Hashtbl.replace taxonomy.items name (data :: items)

let add_taxonomies t page =
  let open Toml.Lenses in
  let open Toml.Types in
  match get page.frontmatter (key "taxonomies" |-- table) with
  | None -> ()
  | Some taxonomies ->
    Table.iter (fun k v ->
        match get v (array |-- strings) with
        | None -> failwith "Expected an array of strings"
        | Some tags ->
          List.iter (fun tag ->
              let jingoo = jingoo_of_page page in
              add_taxonomy t (Table.Key.to_string k) tag jingoo
            ) tags
      ) taxonomies

(* Try to parse YAML frontmatter from the channel. *)
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
      match Toml.Parser.from_string (Buffer.contents buf) with
      | `Ok toml -> toml
      | `Error(e, _) -> failwith e
    else (
      seek_in chan 0;
      failwith "Missing frontmatter"
    )
  with
  | End_of_file ->
    seek_in chan 0;
    failwith "Missing frontmatter"

(* Highlight the code blocks. *)
let highlight t =
  List.map (fun block ->
      match block with
      | Omd.Code_block(lang, code) ->
        begin match lang with
          | "" -> Omd.Code_block("", code)
          | lang ->
            match TmLanguage.find_by_name t.langs lang with
            | None ->
              prerr_endline ("Warning: unknown language " ^ lang);
              Omd.Code_block(lang, code)
            | Some grammar ->
              Omd.Raw
                (Soup.pretty_print
                   (Highlight.highlight_block t.langs grammar code))
        end
      | x -> x)

type doctype =
  | Bin
  | Doc of string doc

(* Copy the file exactly. *)
let copy_file output_path in_chan =
  Filesystem.with_out_bin (fun out_chan ->
      output_string out_chan (Filesystem.read_bytes in_chan)
    ) output_path

let process_md t chan =
  Omd.to_html (highlight t (Omd.of_string (Filesystem.read_lines chan)))

(* Intercepts [Failure _] exceptions to report the file name. *)
let with_in_smart f path =
  try Filesystem.with_in f path with
  | Failure e -> failwith (path ^ ": " ^ e)

let dispatch t subdir name =
  let read_path = Filename.concat subdir name in
  match String.split_on_char '.' name with
  | [name; "lagda"; "md"] ->
    let path =
      if subdir = "" then
        t.config.Config.src_dir ^ "." ^ name
      else
        t.config.Config.src_dir ^ "."
        ^ (String.split_on_char '/' subdir
           |> String.concat (String.make 1 '.'))
        ^ "." ^ name
    in
    with_in_smart (fun chan ->
        let frontmatter = parse_frontmatter chan in
        Doc { name = name ^ ".html"
            ; subdir
            ; frontmatter
            ; content = process_md t chan }
      ) (Filename.concat (Config.agda_dest t.config) path ^ ".md")
  | [_; "html"] ->
    with_in_smart (fun chan ->
        let frontmatter = parse_frontmatter chan in
        Doc { name
            ; subdir
            ; frontmatter
            ; content = Filesystem.read_lines chan }
      ) (Config.src t.config read_path)
  | [name; "md"] ->
    with_in_smart (fun chan ->
        let frontmatter = parse_frontmatter chan in
        Doc { name = name ^ ".html"
            ; subdir
            ; frontmatter
            ; content = process_md t chan }
      ) (Config.src t.config read_path)
  | _ ->
    Filesystem.with_in_bin (fun chan ->
        let output_path = Config.dest t.config read_path in
        copy_file output_path chan
      ) (Config.src t.config read_path);
    Bin

(* Concatenate two URLs, handling trailing slashes on the left URL and
   leading slashes on the right URL. *)
let concat_urls left right =
  let left_len =
    let len = String.length left in
    if len = 0 then
      0
    else if String.get left (len - 1) = '/' then
      len - 1
    else
      len
  in
  let right_start, right_len =
    let len = String.length right in
    if len = 0 then
      (0, 0)
    else if String.get right 0 = '/' then
      (1, len - 1)
    else
      (0, len)
  in
  let bytes = Bytes.create (left_len + right_len + 1) in
  Bytes.blit_string left 0 bytes 0 left_len;
  Bytes.set bytes left_len '/';
  Bytes.blit_string right right_start bytes (left_len + 1) right_len;
  Bytes.unsafe_to_string bytes

let correct_agda_urls t node =
  let open Soup.Infix in
  Soup.iter (fun node ->
      match Soup.attribute "href" node with
      | None -> failwith "Unreachable: href"
      | Some link ->
        let root_mod = t.config.Config.src_dir in
        let root_len = String.length t.config.Config.src_dir in
        let link_len = String.length link in
        if link_len >= root_len && String.sub link 0 root_len = root_mod then
          (* The link is to an internal module *)
          match String.split_on_char '.' link with
          | [] -> failwith "Unreachable: Empty Agda link"
          | _ :: parts ->
            let rec loop acc = function
              | [] -> failwith "Unreachable: Singular Agda link"
              | [ext] -> acc ^ "." ^ ext
              | x :: xs -> loop (acc ^ "/" ^ x) xs
            in
            Soup.set_attribute "href" (loop "" parts) node
        else
          (* The link is to an external module *)
          let link = "/" ^ (concat_urls t.config.Config.agda_dir link) in
          Soup.set_attribute "href" link node
    ) (node $$ "pre[class=\"Agda\"] > a[href]")

let relativize_urls depth node =
  let open Soup.Infix in
  let replace attr =
    Soup.iter (fun node ->
        match Soup.attribute attr node with
        | None -> failwith ("Unreachable: " ^ attr)
        | Some link ->
          if String.get link 0 = '/' then
            let sub = String.sub link 1 (String.length link - 1) in
            let buf = Buffer.create 5 in
            for _ = 1 to depth do
              Buffer.add_string buf "../"
            done;
            Buffer.add_string buf sub;
            Soup.set_attribute attr (Buffer.contents buf) node
      ) (node $$ ("[" ^ attr ^ "]"))
  in
  replace "href";
  replace "src"

let list_page_metadata pages =
  pages
  |> List.filter (fun page -> page.name <> "index.html")
  |> List.map jingoo_of_page

let get_layout frontmatter =
  match Jg_runtime.jg_obj_lookup frontmatter "layout" with
  | Jg_types.Tnull -> None
  | Jg_types.Tstr name -> Some name
  | _ -> failwith "Template name not a string"

let from_file t models path =
  let env =
    { Jg_types.std_env with
      autoescape = false
    ; strict_mode = true
    ; template_dirs = [t.config.partial_dir]
    ; filters =
        [ "format_date"
        , Jg_types.func_arg2_no_kw (fun format date ->
              let open CalendarLib in
              Jg_types.Tstr
                (Printer.Date.sprint
                   (Jg_types.unbox_string format)
                   (Date.from_unixfloat (Jg_types.unbox_float date))))
        ]
    }
  in
  let path = Filename.concat t.config.layout_dir path in
  try Jg_template.from_file ~env ~models path with
  | Failure e -> failwith (path ^ ": " ^ e)
  | Jingoo.Jg_types.SyntaxError e -> failwith (path ^ ": " ^ e)

let render t pages frontmatter content =
  let frontmatter = jingoo_of_tomltable frontmatter in
  match get_layout frontmatter with
  | None -> content
  | Some path ->
    let models =
      [ "content", Jg_types.Tstr content
      ; "posts", Jg_types.Tlist pages
      ; "page", frontmatter ]
    in
    from_file t models path

let compile_doc t depth pages ({ name; subdir; frontmatter; content } as page) =
  let path = Filename.concat subdir name in
  let output_path = Filename.concat t.config.Config.dest_dir path in
  let content = render t pages frontmatter content in
  let output = Soup.parse content in
  add_taxonomies t page;
  correct_agda_urls t output;
  relativize_urls depth output;
  Filesystem.with_out (fun out_chan ->
      output_string out_chan (Soup.pretty_print output)
    ) output_path

let rec compile_dir t depth subdir =
  let src = Config.src t.config subdir in
  let dest = Config.dest t.config subdir in
  Filesystem.touch_dir dest;
  let pages =
    Array.fold_left (fun pages name ->
        let path = Filename.concat subdir name in
        if Sys.is_directory (Config.src t.config path) then (
          compile_dir t (depth + 1) path;
          pages
        ) else if
          List.exists (Fun.flip Re.execp path) t.config.Config.exclude
        then
          pages
        else
          match dispatch t subdir name with
          | Bin -> pages
          | Doc doc -> doc :: pages
      ) [] (Sys.readdir src)
  in
  let metadata = list_page_metadata pages in
  List.iter (compile_doc t depth metadata) pages

(** Highlight Literate Agda files *)
let preprocess_agda html_dir dir =
  let rec go dir =
    Array.iter (fun name ->
        let path = Filename.concat dir name in
        if Sys.is_directory path then
          go path
        else
          match Filename.chop_suffix_opt ~suffix:".lagda.md" name with
          | None -> ()
          | Some _ ->
             let code =
               Sys.command
                 (Filename.quote_command
                    "agda"
                    [ path
                    ; "--html"
                    ; "--html-highlight=auto"
                    ; "--html-dir=" ^ html_dir ])
             in
             if code <> 0 then
               failwith ("Agda process exited with code " ^ Int.to_string code)
      ) (Sys.readdir dir)
  in
  go dir

let build_taxonomy t name taxonomy =
  let dest = Config.dest t.config name in
  Filesystem.touch_dir dest;
  Hashtbl.iter (fun tag_name pages ->
      let output_path = Filename.concat dest tag_name in
      let content =
        from_file t
          [ "posts", Jg_types.Tlist pages
          ; "page", Jg_types.Tobj ["title", Jg_types.Tstr ""] ]
          taxonomy.template
      in
      let output = Soup.parse content in
      correct_agda_urls t output;
      relativize_urls 1 output;
      Filesystem.with_out (fun out_chan ->
          output_string out_chan (Soup.pretty_print output)
        ) output_path
    ) taxonomy.items

let build_taxonomies t =
  Hashtbl.iter (fun name taxonomy ->
      build_taxonomy t name taxonomy
    ) t.taxonomies

let build_with_config config =
  Filesystem.touch_dir config.Config.dest_dir;
  let t =
    { config
    ; langs = TmLanguage.create ()
    ; taxonomies = Hashtbl.create 2 }
  in
  begin
    match Sys.readdir t.config.Config.grammar_dir with
    | exception (Sys_error _) -> ()
    | names ->
       Array.iter (fun name ->
           if Sys.is_directory (Config.grammar t.config name) then
             ()
           else
             let path = Config.grammar t.config name in
             try
               let lang =
                 Filesystem.with_in (fun chan ->
                     Markup.channel chan
                     |> Plist_xml.parse_exn
                     |> TmLanguage.of_plist_exn
                   ) path
               in
               TmLanguage.add_grammar t.langs lang
             with
             | Plist_xml.Parse_error s -> failwith (path ^ ": " ^ s)
         ) names
  end;
  Filesystem.remove_dir t.config.Config.dest_dir;
  List.iter (fun taxonomy ->
      Hashtbl.add t.taxonomies taxonomy.Config.name
        { template = taxonomy.Config.template
        ; items = Hashtbl.create 11 }
    ) config.Config.taxonomies;
  preprocess_agda (Config.agda_dest config) (Config.src config "");
  ignore (compile_dir t 0 "");
  build_taxonomies t

let build () =
  let config =
    match Toml.Parser.from_filename "config.toml" with
    | `Error(e, _) -> failwith e
    | `Ok toml ->
      match Config.of_toml toml with
      | Some config -> config
      | None -> failwith "Could not read config.toml"
  in
  build_with_config config
