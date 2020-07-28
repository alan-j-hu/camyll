type t = {
    input_dir : string;
    output_dir : string;
    highlighting_dir : string;
    layout_dir : string;
    include_dir : string;
    date_rformat : string;
    date_wformat : string;
    exclude : Re.re list;
    agda_dir : string;
  }

let default =
  let fmt = "%B %d, %Y" in
  { input_dir = "site"
  ; output_dir = "public"
  ; highlighting_dir = "highlighting"
  ; layout_dir = "templates"
  ; include_dir = "includes"
  ; date_rformat = fmt
  ; date_wformat = fmt
  ; exclude = List.map (fun r -> Re.compile (Re.Glob.glob r)) ["*.agdai"]
  ; agda_dir = "lagda" }

let src t = Filename.concat t.input_dir

let dest t = Filename.concat t.output_dir

let highlighting t = Filename.concat t.highlighting_dir

let template t = Filename.concat t.layout_dir

let include_ t = Filename.concat t.include_dir

let agda_dest t = Filename.concat t.output_dir t.agda_dir

let of_json : Ezjsonm.value -> t = function
  | `O assoc ->
     let input_dir =
       match List.assoc_opt "source" assoc with
       | None -> default.input_dir
       | Some v -> Ezjsonm.get_string v
     and output_dir =
       match List.assoc_opt "destination" assoc with
       | None -> default.output_dir
       | Some v -> Ezjsonm.get_string v
     and highlighting_dir =
       match List.assoc_opt "highlighting" assoc with
       | None -> default.highlighting_dir
       | Some v -> Ezjsonm.get_string v
     and layout_dir =
       match List.assoc_opt "layout" assoc with
       | None -> default.layout_dir
       | Some v -> Ezjsonm.get_string v
     and include_dir =
       match List.assoc_opt "include" assoc with
       | None -> default.include_dir
       | Some v -> Ezjsonm.get_string v
     and agda_dir =
       match List.assoc_opt "agda" assoc with
       | None -> default.agda_dir
       | Some v -> Ezjsonm.get_string v
     and date_rformat =
       match List.assoc_opt "date_rformat" assoc with
       | None -> default.date_rformat
       | Some v -> Ezjsonm.get_string v
     and date_wformat =
       match List.assoc_opt "date_wformat" assoc with
       | None -> default.date_wformat
       | Some v -> Ezjsonm.get_string v
     and exclude =
       match List.assoc_opt "exclude" assoc with
       | None -> default.exclude
       | Some v ->
          List.map (fun g -> Re.compile (Re.Glob.glob g))
            (Ezjsonm.get_strings v)
     in
     { input_dir
     ; output_dir
     ; highlighting_dir
     ; layout_dir
     ; include_dir
     ; agda_dir
     ; date_rformat
     ; date_wformat
     ; exclude }
  | _ -> failwith "Expected an object"
