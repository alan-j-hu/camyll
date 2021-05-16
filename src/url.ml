let concat_urls left right =
  let left_len =
    let len = String.length left in
    if len = 0 then
      0
    else if left.[len - 1] = '/' then
      len - 1
    else
      len
  in
  let right_start, right_len =
    let len = String.length right in
    if len = 0 then
      (0, 0)
    else if right.[0] = '/' then
      (1, len - 1)
    else
      (0, len)
  in
  let bytes = Bytes.create (left_len + right_len + 1) in
  Bytes.blit_string left 0 bytes 0 left_len;
  Bytes.set bytes left_len '/';
  Bytes.blit_string right right_start bytes (left_len + 1) right_len;
  Bytes.unsafe_to_string bytes

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
  let src, dest = chop_common_prefix src dest in
  match src with
  | [] -> String.concat "/" dest
  | _ :: src ->
    dest
    |> List.rev_append (List.init (List.length src) (Fun.const ".."))
    |> String.concat "/"
