(* If the directory already exists, does nothing. *)
let touch_dir name = if not (Sys.file_exists name) then Sys.mkdir name 0o777
let split_re = Re.compile (Re.str Filename.dir_sep)

(* Creates all directories in the current path. *)
let create_dirs path =
  let split = Re.split split_re path in
  ignore
    (List.fold_left
       (fun path name ->
         let path = Filename.concat path name in
         touch_dir path;
         path)
       "" split)

let rec remove_dir dirname =
  Sys.readdir dirname
  |> Array.iter (fun name -> remove (Filename.concat dirname name));
  Sys.rmdir dirname

and remove name =
  if Sys.is_directory name then remove_dir name else Sys.remove name
