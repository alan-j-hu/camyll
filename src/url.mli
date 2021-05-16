(** Concatenate two URLs, handling trailing slashes on the left URL and
    leading slashes on the right URL. *)
val concat_urls : string -> string -> string

(** Relativizes the absolute URL [dest] assumint it appears on a page at URL
    [src]. *)
val relativize : src:string -> dest:string -> string
