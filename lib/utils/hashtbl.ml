include Stdlib.Hashtbl

let to_list h=
  h |> to_seq |> List.of_seq

let to_array h =
  h |> to_seq |> Array.of_seq