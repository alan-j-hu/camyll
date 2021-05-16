open Camyll.Url

let () =
  assert (relativize ~src:"/posts/first-post/" ~dest:"/posts/" = "../");
  assert (relativize ~src:"/posts/" ~dest:"/posts/first-post/" = "first-post/");
  assert
    (relativize ~src:"/posts/first-post/" ~dest:"/posts/second-post/"
     = "../second-post/");
