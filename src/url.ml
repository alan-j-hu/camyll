let relativize ~src ~dest =
  let chop_common_prefix url1 url2 =
    let url1 = String.split_on_char '/' url1 in
    let url2 = String.split_on_char '/' url2 in
    let rec loop url1 url2 = match url1, url2 with
      | x :: xs, y :: ys when x = y -> loop xs ys
      | url1, url2 -> url1, url2
    in
    loop url1 url2
  in
  if String.length dest > 0 && String.get dest 0 = '/' then
    let src, dest = chop_common_prefix src dest in
    match src with
    | [] -> String.concat "/" dest
    | _ :: src ->
      dest
      |> List.rev_append (List.init (List.length src) (Fun.const ".."))
      |> String.concat "/"
  else
    dest
