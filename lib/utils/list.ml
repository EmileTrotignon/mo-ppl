include Stdlib.List

let rec append_n li1 n li2 =
  if n = 0 then li2
  else
    match li1 with
    | [] ->
        raise (Invalid_argument "n must be smaller than List.length li1")
    | e :: li1 ->
        e :: append_n li1 (n - 1) li2

let sub li i n =
  li |> Array.of_list |> (fun li -> Array.sub li i n) |> Array.to_list

let fold_right_map f init li =
  let li = rev li in
  let r, li = fold_left_map f init li in
  (r, rev li)
