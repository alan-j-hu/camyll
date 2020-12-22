open Cmdliner

let new_cmd =
  let doc = "initialize a site" in
  let arg =
    let inf = Arg.info ~docv:"PATH" [] in
    Arg.(required & pos 0 (some string) None inf)
  in
  Term.(ret (const New.new_ $ arg)),
  Term.info "new" ~doc

let build_cmd =
  let doc = "build the site" in
  Term.(ret (const Build.build $ const ())),
  Term.info "build" ~doc

let default_cmd =
  let doc = "static site generator" in
  Term.(ret (const (`Help(`Auto, None)))),
  Term.info "camyll" ~doc

let cmds = [new_cmd; build_cmd]

let main () = Term.(exit @@ eval_choice default_cmd cmds)
