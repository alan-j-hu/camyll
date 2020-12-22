let split_re = Re.compile (Re.str Filename.dir_sep)

let new_ path =
  try
    let split = Re.split split_re path in
    let _ =
      List.fold_left (fun path name ->
          let path = Filename.concat path name in
          Filesystem.mkdir path;
          path
        ) "" split
    in
    Filesystem.mkdir (Filename.concat path "site");
    Filesystem.mkdir (Filename.concat path "grammars");
    Filesystem.mkdir (Filename.concat path "layouts");
    Filesystem.mkdir (Filename.concat path "partials");
    `Ok ()
  with
  | Failure e -> `Error (false, e)
