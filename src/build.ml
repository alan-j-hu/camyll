open Jingoo

type 'a doc = {
  name : string;
  subdir : string;
  frontmatter : Yaml.yaml;
  content : 'a;
}

type t = {
  config : Config.t;
  langs : TmLanguage.t;
}

(* The null YAML document. *)
let null_yaml = `Scalar Yaml.{
    anchor = None;
    tag = None;
    value = "null";
    plain_implicit = true;
    quoted_implicit = true;
    style = `Literal
  }

(* Try to parse YAML frontmatter from the channel. If there is no frontmatter,
   reset the cursor to the beginning of the file and return the null YAML
   document. *)
let parse_frontmatter chan =
  try
    let line = input_line chan in
    if line = "---" then
      let buf = Buffer.create 100 in
      let rec loop () =
        let line = input_line chan in
        if line = "---" then
          ()
        else (
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
        )
      in
      loop ();
      match Yaml.yaml_of_string (Buffer.contents buf) with
      | Ok yaml -> yaml
      | Error (`Msg e) -> failwith e
    else (
      seek_in chan 0;
      null_yaml
    )
  with
  | End_of_file ->
    seek_in chan 0;
    null_yaml

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

let dispatch t subdir name =
  match Filename.chop_suffix_opt ~suffix:".lagda.md" name with
  | Some name ->
    let path =
      if subdir = "" then
        t.config.Config.src_dir ^ "." ^ name
      else
        t.config.Config.src_dir ^ "."
        ^ (String.split_on_char '/' subdir
           |> String.concat (String.make 1 '.'))
        ^ "." ^ name
    in
    Filesystem.with_in_bin (fun chan ->
        let frontmatter = parse_frontmatter chan in
        Doc { name = name ^ ".html"
            ; subdir
            ; frontmatter
            ; content = process_md t chan }
      ) (Filename.concat (Config.agda_dest t.config) path ^ ".md")
  | None ->
    let path = Filename.concat subdir name in
    match Filename.chop_suffix_opt ~suffix:".html" name with
    | Some _ ->
      Filesystem.with_in_bin (fun chan ->
          let frontmatter = parse_frontmatter chan in
          Doc { name
              ; subdir
              ; frontmatter
              ; content = Filesystem.read_lines chan }
        ) (Config.src t.config path)
    | None ->
      match Filename.chop_suffix_opt ~suffix:".md" name with
      | Some name ->
        Filesystem.with_in_bin (fun chan ->
            let frontmatter = parse_frontmatter chan in
            Doc { name = name ^ ".html"
                ; subdir
                ; frontmatter
                ; content = process_md t chan }
          ) (Config.src t.config path)
      | None ->
        Filesystem.with_in_bin (fun chan ->
            let output_path = Config.dest t.config path in
            copy_file output_path chan
          ) (Config.src t.config path);
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
          | [] -> failwith "unreachable"
          | _ :: parts ->
            let rec loop acc = function
              | [] -> failwith "unreachable"
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

let rec jingoo_of_yaml = function
  | `Scalar scalar ->
    begin match scalar.Yaml.value with
      | "" | "~" | "null" | "Null" | "NULL" -> Jg_types.Tnull
      | "y" | "Y" | "yes" | "Yes" | "YES"
      | "true" | "True" | "TRUE"
      | "on" | "On" | "ON" -> Jg_types.Tbool true
      | "n" | "N" | "no" | "No" | "NO"
      | "false" | "False" | "FALSE"
      | "off" | "Off" | "OFF" -> Jg_types.Tbool false
      | "nan" | "NaN" | "NAN" -> Jg_types.Tfloat Float.nan
      | "-.inf" -> Jg_types.Tfloat Float.neg_infinity
      | ".inf" -> Jg_types.Tfloat Float.infinity
      | s ->
        match int_of_string_opt s with
        | Some i -> Jg_types.Tint i
        | None ->
          match float_of_string_opt s with
          | Some f -> Jg_types.Tfloat f
          | None -> Jg_types.Tstr s
    end
  | `A elems -> Jg_types.Tlist (List.map jingoo_of_yaml elems)
  | `O attrs ->
    Jg_types.Tobj
      (List.map (fun (k, v) -> (k.Yaml.value, jingoo_of_yaml v)) attrs)
  | `Alias _ -> failwith "YAML aliases not supported!"

let list_page_metadata pages =
  pages
  |> List.filter (fun page -> page.name <> "index.html")
  |> List.map (fun page ->
      Jg_types.Tobj [ ("url", Jg_types.Tstr page.name)
                    ; ("frontmatter", jingoo_of_yaml page.frontmatter) ])

let get_layout _t frontmatter =
  match List.assoc_opt "layout" (Jg_types.unbox_obj frontmatter) with
  | None -> None
  | Some (Jg_types.Tstr name) -> Some name
  | Some _ -> failwith "Template name not a string"

let render t pages frontmatter content =
  let frontmatter = jingoo_of_yaml frontmatter in
  match get_layout t frontmatter with
  | None -> content
  | Some path ->
    let env =
      { Jg_types.std_env with
        autoescape = false
      ; strict_mode = true
      ; template_dirs = [t.config.partial_dir]
      ; filters =
          [ "read_date"
          , Jg_types.func_arg2_no_kw (fun format string ->
                let open CalendarLib in
                let date =
                  Printer.Date.from_fstring
                    (Jg_types.unbox_string format)
                    (Jg_types.unbox_string string)
                in
                Jg_types.Tpat (function
                    | "year" ->
                      Jg_types.Tint (Date.year date)
                    | "month" ->
                      Jg_types.Tint
                        (Date.int_of_month (Date.month date))
                    | "day" ->
                      Jg_types.Tint (Date.day_of_month date)
                    | "unix" ->
                      Jg_types.Tfloat (Date.to_unixfloat date)
                    | "format" ->
                      Jg_types.func_arg1_no_kw (fun format ->
                          Jg_types.Tstr
                            (Printer.Date.sprint
                               (Jg_types.unbox_string format) date)
                        )
                    | _ -> Jg_types.Tnull)
              )
          ] } in
    let models =
      [ "content", Jg_types.Tstr content
      ; "posts", Jg_types.Tlist pages
      ; "page", frontmatter ]
    in
    Jg_template.from_file ~env ~models
      (Filename.concat t.config.layout_dir path)

let compile_doc t depth pages { name; subdir; frontmatter; content } =
  let path = Filename.concat subdir name in
  let output_path = Filename.concat t.config.Config.dest_dir path in
  let content = render t pages frontmatter content in
  let output = Soup.parse content in
  correct_agda_urls t output;
  relativize_urls depth output;
  Filesystem.with_out (fun out_chan ->
      output_string out_chan (Soup.pretty_print output)
    ) output_path

let rec compile_dir t depth subdir =
  let src = Config.src t.config subdir in
  let dest = Config.dest t.config subdir in
  Filesystem.mkdir dest;
  let pages =
    Filesystem.fold (fun pages name ->
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
      ) [] src
  in
  let metadata = list_page_metadata pages in
  List.iter (compile_doc t depth metadata) pages

(** Highlight Literate Agda files *)
let preprocess_agda html_dir dir =
  let rec spawn_processes pids dir =
    Filesystem.fold (fun pids name ->
        let path = Filename.concat dir name in
        if Sys.is_directory path then
          spawn_processes pids path
        else
          match Filename.chop_suffix_opt ~suffix:".lagda.md" name with
          | None -> pids
          | Some _ ->
            let pid =
              Unix.create_process "agda"
                [| "agda"; path; "--html"; "--html-highlight=auto"
                 ; "--html-dir=" ^ html_dir |]
                Unix.stdin Unix.stdout Unix.stderr
            in pid :: pids
      ) pids dir
  in
  spawn_processes [] dir
  |> List.map (Unix.waitpid [])
  |> List.iter (function
      | _, Unix.WEXITED 0 -> ()
      | _, Unix.WEXITED n ->
        failwith ("Exit code: " ^ Int.to_string n)
      | _, Unix.WSIGNALED _ ->
        failwith "Killed by signal"
      | _, Unix.WSTOPPED _ ->
        failwith "Stopped by signal")

let build config =
  Filesystem.mkdir config.Config.dest_dir;
  let t = { config; langs = TmLanguage.create () } in
  begin
    try
      Filesystem.iter (fun name ->
          if Sys.is_directory (Config.grammar t.config name) then
            ()
          else
            try
              let lang =
                Filesystem.with_in (fun chan ->
                    Markup.channel chan
                    |> Plist_xml.parse_exn
                    |> TmLanguage.of_plist_exn
                  ) (Config.grammar t.config name)
              in
              TmLanguage.add_grammar t.langs lang
            with
            | Plist_xml.Parse_error s ->
              failwith ("Parse_error " ^ s ^ ": " ^ name)
        ) t.config.Config.grammar_dir
    with Unix.Unix_error(Unix.ENOENT, "opendir", dir)
      when dir = t.config.Config.grammar_dir -> ()
  end;
  Filesystem.remove_dir t.config.Config.dest_dir;
  preprocess_agda (Config.agda_dest config) (Config.src config "");
  ignore (compile_dir t 0 "")
