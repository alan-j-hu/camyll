(** Relativizes the absolute URL [dest] assumint it appears on a page at URL
    [src]. *)
val relativize : src:string -> dest:string -> string
