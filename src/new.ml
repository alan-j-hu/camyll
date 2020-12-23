let new_ path =
  Filesystem.create_dirs path;
  Filesystem.touch_dir (Filename.concat path "site");
  Filesystem.touch_dir (Filename.concat path "grammars");
  Filesystem.touch_dir (Filename.concat path "layouts");
  Filesystem.touch_dir (Filename.concat path "partials")
