open Camyll.Url

let () =
  assert (concat_urls "/" "/" = "/");
  assert (concat_urls "/foo/bar" "/baz" = "/foo/bar/baz");
  assert (concat_urls "/foo/bar/" "baz" = "/foo/bar/baz");
  assert (concat_urls "/foo/bar" "baz" = "/foo/bar/baz");
  assert (concat_urls "/foo/bar" "" = "/foo/bar/");
  assert (concat_urls "/foo/bar/" "" = "/foo/bar/")

let () =
  assert (relativize ~src:"/posts/first-post/" ~dest:"/posts/" = "../");
  assert (relativize ~src:"/posts/" ~dest:"/posts/first-post/" = "first-post/");
  assert
    (relativize ~src:"/posts/first-post/" ~dest:"/posts/second-post/"
     = "../second-post/");
