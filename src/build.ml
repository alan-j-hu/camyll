type 'a doc = {
    name : string;
    subdir : string;
    frontmatter : Yaml.value option;
    content : 'a;
  }

type t = {
    config : Config.t;
    languages : (string, Highlight.tm_grammar) Hashtbl.t;
    templates : (string, Mustache.t) Hashtbl.t;
    includes : (string, Mustache.t) Hashtbl.t;
  }

let (let+) res f = Result.map f res

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
      match Yaml.of_string (Buffer.contents buf) with
      | Ok yaml -> Ok (Some yaml)
      | Error (`Msg e) -> Error e
    else (
      seek_in chan 0;
      Ok None
    )
  with
  | End_of_file ->
     seek_in chan 0;
     Ok None

(** Highlight the code blocks *)
let highlight t =
  List.map (fun block ->
      let bl_desc =
        match block.Omd.bl_desc with
        | Omd.Code_block(lang, code) ->
           begin match lang with
           | "" -> Omd.Code_block("", code)
           | lang ->
              match
                Hashtbl.find_opt t.languages (String.lowercase_ascii lang)
              with
              | None ->
                 prerr_endline ("Warning: unknown language " ^ lang);
                 Omd.Code_block(lang, code)
              | Some grammar ->
                 Omd.Html_block
                   (Soup.pretty_print (Highlight.tokenize_block grammar code))
           end
        | x -> x
      in
      Omd.{ bl_desc; bl_attributes = [] })

type doctype =
  | Bin
  | Doc of string doc

let copy_file output_path in_chan =
  (* Copy file exactly *)
  Filesystem.with_out_bin (fun out_chan ->
      output_string out_chan (Filesystem.read_bytes in_chan)
    ) output_path

let dispatch t subdir name =
  match Filename.chop_suffix_opt ~suffix:".lagda.md" name with
  | Some name ->
     let path =
       if subdir = "" then
         t.config.Config.input_dir ^ "." ^ name
       else
         t.config.Config.input_dir ^ "."
         ^ (String.split_on_char '/' subdir
            |> String.concat (String.make 1 '.'))
         ^ "." ^ name
     in
     Filesystem.with_in_bin (fun chan ->
         let+ frontmatter = parse_frontmatter chan in
         Doc { name = name ^ ".html"
             ; subdir
             ; frontmatter
             ; content = Omd.to_html (highlight t (Omd.of_channel chan)) }
       ) (Filename.concat (Config.agda_dest t.config) path ^ ".md")
  | None ->
     let path = Filename.concat subdir name in
     match Filename.chop_suffix_opt ~suffix:".html" name with
     | Some _ ->
        Filesystem.with_in_bin (fun chan ->
            let+ frontmatter = parse_frontmatter chan in
            Doc { name
                ; subdir
                ; frontmatter
                ; content = Filesystem.read_lines chan }
          ) (Config.src t.config path)
     | None ->
        match Filename.chop_suffix_opt ~suffix:".md" name with
        | Some name ->
           Filesystem.with_in_bin (fun chan ->
               let+ frontmatter = parse_frontmatter chan in
               Doc { name = name ^ ".html"
                   ; subdir
                   ; frontmatter
                   ; content = Omd.to_html (highlight t (Omd.of_channel chan)) }
             ) (Config.src t.config path)
        | None ->
           Filesystem.with_in_bin (fun chan ->
               let output_path = Config.dest t.config path in
               copy_file output_path chan
             ) (Config.src t.config path);
           Ok Bin

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
  let right_len =
    let len = String.length right in
    if len = 0 then
      0
    else if String.get right 0 = '/' then
      len - 1
    else
      len
  in
  let bytes = Bytes.create (left_len + right_len + 1) in
  Bytes.blit_string left 0 bytes 0 left_len;
  Bytes.set bytes left_len '/';
  Bytes.blit_string right 0 bytes (left_len + 1) right_len;
  Bytes.unsafe_to_string bytes

let correct_agda_urls t node =
  let open Soup.Infix in
  Soup.iter (fun node ->
      match Soup.attribute "href" node with
      | None -> failwith "Unreachable: href"
      | Some link ->
         let root_mod = t.config.Config.input_dir in
         let root_len = String.length t.config.Config.input_dir in
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

let list_page_metadata t pages =
  List.filter_map (fun page ->
      Option.bind page.frontmatter (fun yaml ->
          let obj = Ezjsonm.get_dict yaml in
          Option.map (fun published ->
              let date =
                published
                |> Ezjsonm.get_string
                |> CalendarLib.Printer.Date.from_fstring
                     t.config.Config.date_rformat
              in (date, page.name, obj))
            (Highlight.find "published" obj)
        )
    ) pages
  |> List.sort (fun (d1, _, _) (d2, _, _) -> CalendarLib.Date.compare d2 d1)
  |> List.map (fun (_, url, obj) -> `O (("url", `String url) :: obj))

let render ~partials template yaml content =
  Mustache.render ~partials template (`O (("content", `String content) :: yaml))

let get_template t frontmatter =
  match frontmatter with
  | Some (`O attrs) ->
     begin match List.assoc_opt "template" attrs with
     | None -> None
     | Some (`String name) -> Some (Hashtbl.find t.templates name)
     | Some _ -> failwith "Template name not a string"
     end
  | Some _ -> failwith "Frontmatter not an object"
  | None -> None

let compile_doc t depth pages { name; subdir; frontmatter; content } =
  let path = Filename.concat subdir name in
  let output_path = Filename.concat t.config.Config.output_dir path in
  let yaml = match frontmatter with
    | Some yaml -> ["pages", `A pages; "page", yaml]
    | None -> ["pages", `A pages]
  in
  let content = match get_template t frontmatter with
    | Some template ->
       render ~partials:(Hashtbl.find_opt t.includes) template yaml content
    | None -> content
  in
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
        ) else
          if List.exists (Fun.flip Re.execp path) t.config.Config.exclude then
            pages
          else
            match dispatch t subdir name with
            | Error e -> failwith e
            | Ok Bin -> pages
            | Ok (Doc doc) -> doc :: pages
      ) [] src
  in
  let metadata = list_page_metadata t pages in
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

let build () =
  let config = Config.default in
  Filesystem.mkdir config.output_dir;
  let templates = Hashtbl.create 11 in
  Filesystem.iter (fun filename ->
      if Sys.is_directory (Config.template config filename) then
        ()
      else
        let name = Filename.chop_suffix filename ".html" in
        try
          Filesystem.read (Config.template config filename)
          |> Mustache.of_string
          |> Hashtbl.add templates name
        with
        | Invalid_argument s ->
           failwith ("Invalid_argument " ^ s ^ ": " ^ name)
    ) config.Config.template_dir;
  let includes = Hashtbl.create 11 in
  Filesystem.iter (fun name ->
      if Sys.is_directory (Config.include_ config name) then
        ()
      else
        let s = Filesystem.read (Config.include_ config name) in
        let name = Filename.chop_suffix name ".html" in
        Hashtbl.add includes name (Mustache.of_string s)
    ) config.Config.include_dir;
  let t =
    { config
    ; languages = Hashtbl.create 10
    ; templates
    ; includes }
  in
  begin
    try
      Filesystem.iter (fun name ->
          if Sys.is_directory (Config.highlighting t.config name) then
            ()
          else
            try
              Filesystem.with_in (fun chan ->
                  let lang =
                    Markup.channel chan
                    |> Highlight.plist_of_xml
                    |> Highlight.of_plist
                  in
                  Hashtbl.add t.languages
                    (String.lowercase_ascii lang.Highlight.name) lang
                ) (Config.highlighting t.config name)
            with
            | Highlight.Parse_error s ->
               failwith ("Parse_error " ^ s ^ ": " ^ name)
        ) t.config.Config.highlighting_dir
    with Unix.Unix_error(Unix.ENOENT, "opendir", "highlighting") -> ()
  end;
  Filesystem.remove_dir t.config.Config.output_dir;
  preprocess_agda (Config.agda_dest config) (Config.src config "");
  ignore (compile_dir t 0 "")
