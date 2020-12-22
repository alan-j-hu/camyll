let new_ path =
  try
    Filesystem.create_dirs path;
    Filesystem.mkdir (Filename.concat path "site");
    Filesystem.mkdir (Filename.concat path "grammars");
    Filesystem.mkdir (Filename.concat path "layouts");
    Filesystem.mkdir (Filename.concat path "partials");
    `Ok ()
  with
  | Failure e -> `Error (false, e)
