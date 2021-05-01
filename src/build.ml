open Jingoo

type name =
  | Index
  | Name of string

type page = {
  frontmatter : Toml.Types.table;
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
  template : string;
  items : (string, Jg_types.tvalue list) Hashtbl.t;
}

type t = {
  config : Config.t;
  langs : TmLanguage.t;
  taxonomies : (string, taxonomy) Hashtbl.t;
}

let slugify str =
  let buf = Buffer.create (String.length str) in
  String.iter (function
      | ' ' -> Buffer.add_char buf '-'
      | 'A' .. 'Z' as ch -> Buffer.add_char buf (Char.chr (Char.code ch + 32))
      | 'a' .. 'z' | '-' | '_' as ch -> Buffer.add_char buf ch
      | _ -> ()
    ) str;
  Buffer.contents buf

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

let jingoo_of_page url page =
  Jg_types.Tobj
    [ ("url", Jg_types.Tstr url)
    ; ("frontmatter", jingoo_of_tomltable page.frontmatter) ]

(* Add the Jingoo data to the taxonomy under the specified name. *)
let add_taxonomy t taxonomy name data =
  match Hashtbl.find_opt t.taxonomies taxonomy with
  | None -> failwith ("Taxonomy " ^ taxonomy ^ " not defined")
  | Some taxonomy ->
    let name = slugify name in
    match Hashtbl.find_opt taxonomy.items name with
    | None -> Hashtbl.add taxonomy.items name [data]
    | Some items -> Hashtbl.replace taxonomy.items name (data :: items)

let add_taxonomies t url page =
  let open Toml.Lenses in
  let open Toml.Types in
  match get page.frontmatter (key "taxonomies" |-- table) with
  | None -> ()
  | Some taxonomies ->
    Table.iter (fun taxonomy v ->
        match get v (array |-- strings) with
        | None -> failwith "Expected an array of strings"
        | Some tags ->
          List.iter (fun tag ->
              let jingoo = jingoo_of_page url page in
              add_taxonomy t (Table.Key.to_string taxonomy) tag jingoo
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

let process_md t chan =
  Omd.to_html (highlight t (Omd.of_string (Filesystem.read_lines chan)))

(* Intercepts [Failure _] exceptions to report the file name. *)
let with_in_smart f path =
  try Filesystem.with_in f path with
  | Failure e -> failwith (path ^ ": " ^ e)

let dispatch t path name =
  let read_path = Filename.concat path name in
  match String.split_on_char '.' name with
  | ["index"; "html"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      ( Index
      , Page { frontmatter
             ; content = Filesystem.read_lines chan } )
    end
  | ["index"; "md"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      ( Index
      , Page { frontmatter
             ; content = process_md t chan } )
    end
  | [name; "lagda"; "md"] ->
    let module_name =
      (String.split_on_char '/' path |> String.concat ".") ^ "." ^ name
    in
    let exit_code =
      Sys.command
        (Filename.quote_command
           "agda"
           [ read_path
           ; "--html"
           ; "--html-highlight=auto"
           ; "--html-dir=" ^ Config.agda_dest t.config ])
    in
    if exit_code <> 0 then
      failwith ("Agda process exited with code " ^ Int.to_string exit_code)
    else
      Filename.concat (Config.agda_dest t.config) module_name ^ ".md" |>
      with_in_smart begin fun chan ->
        let frontmatter = parse_frontmatter chan in
        ( Name name
        , Page { frontmatter
               ; content = process_md t chan } )
      end
  | [name; "html"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      ( Name name
      , Page { frontmatter
             ; content = Filesystem.read_lines chan } )
    end
  | [name; "md"] ->
    read_path |> with_in_smart begin fun chan ->
      let frontmatter = parse_frontmatter chan in
      ( Name name
      , Page { frontmatter
             ; content = process_md t chan } )
    end
  | _ ->
    read_path |> Filesystem.with_in_bin begin fun chan ->
      (Name name, Bin (Filesystem.read_bytes chan))
    end

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

let render_from_file t models url path =
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
  let print_err e = failwith (path ^ ": " ^ url ^ ":" ^ e) in
  try Jg_template.from_file ~env ~models path with
  | Failure e -> failwith (print_err e)
  | Invalid_argument e -> failwith (print_err e)
  | Jingoo.Jg_types.SyntaxError e -> failwith (print_err e)

let render_page t siblings url page =
  match Toml.Lenses.(get page.frontmatter (key "layout" |-- string)) with
  | None -> page.content
  | Some path ->
    let models =
      [ "content", Jg_types.Tstr page.content
      ; "posts", Jg_types.Tlist siblings
      ; "page", jingoo_of_tomltable page.frontmatter ]
    in
    render_from_file t models url path

let compile_page t depth siblings path url page =
  let content = render_page t siblings url page in
  let output = Soup.parse content in
  add_taxonomies t url page;
  correct_agda_urls t output;
  relativize_urls depth output;
  path |> Filesystem.with_out begin fun out_chan ->
    output_string out_chan (Soup.pretty_print output)
  end

let rec load_dir t src =
  let files = Sys.readdir src in
  let pages = Hashtbl.create (Array.length files) in
  let index = Array.fold_left (fun index name ->
      let path = Filename.concat src name in
      if List.exists (Fun.flip Re.execp path) t.config.Config.exclude then
        index
      else if Sys.is_directory path then (
        let dir = load_dir t path in
        if Hashtbl.mem pages name then
          failwith ("Duplicate page " ^ path ^ "!")
        else (
          Hashtbl.add pages name (Dir dir);
          index
        )
      ) else
        let name, data = dispatch t src name in
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
    ) None (Sys.readdir src)
  in
  { dir_page = index; children = pages }

let rec compile_dir t depth root url { dir_page; children } =
  let pages =
    Hashtbl.to_seq children
    |> Seq.filter_map (function
        | name, Page page ->
          Some (jingoo_of_page (url ^ "/" ^ name ^ ".html") page)
        | _, _ -> None)
    |> List.of_seq
  in
  Filesystem.touch_dir root;
  Hashtbl.iter (fun name item ->
      match item with
      | Bin data ->
        let dest = Filename.concat root name in
        Filesystem.with_out_bin (Fun.flip output_string data) dest
      | Dir subdir ->
        compile_dir
          t (depth + 1) (Filename.concat root name) (url ^ "/" ^ name) subdir
      | Page page ->
        let dest = Filename.concat root name ^ ".html" in
        compile_page t depth pages dest (url ^ "/" ^ name ^ ".html") page
    ) children;
  match dir_page with
  | None -> ()
  | Some page ->
    compile_page t depth pages (Filename.concat root "index.html") url page

let build_taxonomy t name taxonomy =
  let dest = Filename.concat t.config.Config.dest_dir name in
  Filesystem.touch_dir dest;
  Hashtbl.iter (fun tag_name pages ->
      let output_path = Filename.concat dest (slugify tag_name ^ ".html") in
      let content =
        render_from_file t
          [ "posts", Jg_types.Tlist pages
          ; "page", Jg_types.Tobj ["title", Jg_types.Tstr tag_name] ]
          dest
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
  let dir = load_dir t t.config.Config.src_dir in
  compile_dir t 0 t.config.Config.dest_dir "" dir;
  build_taxonomies t

let build () = Config.with_config build_with_config
