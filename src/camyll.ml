open Cmdliner

let build_cmd =
  let doc = "build the site" in
  Term.(ret (const Build.build $ const ())),
  Term.info "build" ~doc

let default_cmd =
  let doc = "static site generator" in
  Term.(ret (const (`Help(`Auto, None)))),
  Term.info "camyll" ~doc

let cmds = [build_cmd]

let main () = Term.(exit @@ eval_choice default_cmd cmds)
