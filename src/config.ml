module Otoml = Otoml_impl.T

type taxonomy = {
  name : string;
  layout : string;
}

type t = {
  dest_dir : string;
  exclude : Re.re list;
  agda_dir : string;
  taxonomies : taxonomy list;
}

let agda_dest t = Filename.concat t.dest_dir t.agda_dir

let (let+) opt f = match opt with
  | Some x -> Some (f x)
  | None -> None

let (and+) lhs rhs = match lhs, rhs with
  | Some lhs, Some rhs -> Some (lhs, rhs)
  | _, _ -> None

let (and*) = (and+)

let (let*) opt f = match opt with
  | Some x -> f x
  | None -> None

let rec mapM f = function
  | [] -> Some []
  | x :: xs ->
    match f x with
    | None -> None
    | Some y ->
      match mapM f xs with
      | None -> None
      | Some xs -> Some (y :: xs)

let taxonomy_of_toml toml =
  let+ name = Otoml.find_opt toml Otoml.get_string ["name"]
  and+ layout = Otoml.find_opt toml Otoml.get_string ["layout"] in
  { name; layout }

let of_toml toml =
  let* dest_dir = Otoml.find_opt toml Otoml.get_string ["dest_dir"]
  and* agda_dir = Otoml.find_opt toml Otoml.get_string ["agda_dir"]
  and* exclude = Otoml.find_opt toml (Otoml.get_array Otoml.get_string) ["exclude"]
  and* taxonomies =
    Otoml.find_opt toml (Otoml.get_array Otoml.get_value) ["taxonomies"]
  in
  let+ taxonomies = mapM taxonomy_of_toml taxonomies in
  { dest_dir
  ; agda_dir
  ; exclude = List.map (fun g -> Re.compile (Re.Glob.glob g)) exclude
  ; taxonomies }

let with_config f =
  let config =
    match Otoml.Parser.from_file_result "config.toml" with
    | Error e -> failwith e
    | Ok toml ->
      match of_toml toml with
      | Some config -> config
      | None -> failwith "Could not read config.toml"
  in
  f config
