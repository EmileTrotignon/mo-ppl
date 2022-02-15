include Stdlib.Array

let map_snd f a = map (fun (a, b) -> (a, f b)) a

let fold_left_snd f = fold_left (fun acc (_, e) -> f acc e)

let assoc a ~key =
  a |> find_opt (fun (key', _) -> key = key') |> Option.map snd

let assoc_exn a ~key =
  a |> find_opt (fun (key', _) -> key = key') |> Option.get |> snd
