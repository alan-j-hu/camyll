type t = {
    input_dir : string;
    output_dir : string;
    highlighting_dir : string;
    template_dir : string;
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
  ; template_dir = "templates"
  ; include_dir = "includes"
  ; date_rformat = fmt
  ; date_wformat = fmt
  ; exclude = List.map (fun r -> Re.compile (Re.Glob.glob r)) ["*.agdai"]
  ; agda_dir = "lagda" }

let src t = Filename.concat t.input_dir

let dest t = Filename.concat t.output_dir

let highlighting t = Filename.concat t.highlighting_dir

let template t = Filename.concat t.template_dir

let include_ t = Filename.concat t.include_dir

let agda_dest t = Filename.concat t.output_dir t.agda_dir
