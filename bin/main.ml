open Camyll
open Cmdliner

let guard f x =
  try Ok (f x) with
  | Failure e -> Error e
  | Invalid_argument e -> Error e
  | Sys_error e -> Error e
  | Unix.Unix_error (e, cmd, "") -> Error (cmd ^ ": " ^ Unix.error_message e)
  | Unix.Unix_error (e, cmd, p) ->
    Error (cmd ^ " " ^ p ^ ": " ^ Unix.error_message e)

let build_cmd =
  let doc = "build the site" in
  Cmd.v (Cmd.info "build" ~doc)
    Term.(term_result' (const (guard Build.build) $ const ()))

let serve_cmd =
  let doc = "serve the site" in
  let port =
    let inf = Arg.info ~docv:"PORT" [ "port" ] in
    Arg.(value & opt int 8080 inf)
  in
  Cmd.v (Cmd.info "serve" ~doc)
    Term.(term_result' (const (guard Serve.serve) $ port))

let () =
  let doc = "static site generator" in
  let cmds = [ build_cmd; serve_cmd ] in
  exit Cmd.(eval (group (Cmd.info "camyll" ~doc) cmds))
