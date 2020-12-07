type t = {
  src_dir : string;
  dest_dir : string;
  grammar_dir : string;
  layout_dir : string;
  partial_dir : string;
  exclude : Re.re list;
  agda_dir : string;
}

let default =
  { src_dir = "site"
  ; dest_dir = "public"
  ; grammar_dir = "grammars"
  ; layout_dir = "layouts"
  ; partial_dir = "partials"
  ; exclude = List.map (fun r -> Re.compile (Re.Glob.glob r)) ["*.agdai"]
  ; agda_dir = "lagda" }

let src t = Filename.concat t.src_dir

let dest t = Filename.concat t.dest_dir

let grammar t = Filename.concat t.grammar_dir

let layout t = Filename.concat t.layout_dir

let partial t = Filename.concat t.partial_dir

let agda_dest t = Filename.concat t.dest_dir t.agda_dir

let get_array = function
  | `A arr -> arr
  | _ -> failwith "Expected an array"

let get_string = function
  | `Scalar s -> s.Yaml.value
  | _ -> failwith "Expected a string"

let of_yaml : Yaml.yaml -> t = function
  | `O assoc ->
    let assoc = List.map (fun (k, v) -> (k.Yaml.value, v)) assoc in
    let src_dir =
      match List.assoc_opt "src_dir" assoc with
      | None -> default.src_dir
      | Some v -> get_string v
    and dest_dir =
      match List.assoc_opt "dest_dir" assoc with
      | None -> default.dest_dir
      | Some v -> get_string v
    and grammar_dir =
      match List.assoc_opt "grammar_dir" assoc with
      | None -> default.grammar_dir
      | Some v -> get_string v
    and layout_dir =
      match List.assoc_opt "layout_dir" assoc with
      | None -> default.layout_dir
      | Some v -> get_string v
    and partial_dir =
      match List.assoc_opt "partial_dir" assoc with
      | None -> default.partial_dir
      | Some v -> get_string v
    and agda_dir =
      match List.assoc_opt "agda_dir" assoc with
      | None -> default.agda_dir
      | Some v -> get_string v
    and exclude =
      match List.assoc_opt "exclude" assoc with
      | None -> default.exclude
      | Some v ->
        List.map (fun g -> Re.compile (Re.Glob.glob g))
          (List.map get_string (get_array v))
    in
    { src_dir
    ; dest_dir
    ; grammar_dir
    ; layout_dir
    ; partial_dir
    ; agda_dir
    ; exclude }
  | _ -> failwith "Expected an object"
