module ISODate = struct
  type t = float

  let parse date = fst (ISO8601.Permissive.datetime_tz ~reqtime:false date)

  let local_time_of_string = parse
  let local_date_of_string = parse
  let local_datetime_of_string = parse
  let offset_datetime_of_string = parse

  let local_time_to_string = ISO8601.Permissive.string_of_time
  let local_date_to_string = ISO8601.Permissive.string_of_date
  let local_datetime_to_string = ISO8601.Permissive.string_of_datetime
  let offset_datetime_to_string = ISO8601.Permissive.string_of_datetime
end

module T = Otoml.Base.Make (Otoml.Base.OCamlNumber) (ISODate)


