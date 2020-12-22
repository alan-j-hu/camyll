let new_ path =
  Filesystem.create_dirs path;
  Filesystem.mkdir (Filename.concat path "site");
  Filesystem.mkdir (Filename.concat path "grammars");
  Filesystem.mkdir (Filename.concat path "layouts");
  Filesystem.mkdir (Filename.concat path "partials")
