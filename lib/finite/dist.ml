open Utils

type 'a t = ('a * Prob.t) array

let bernoulli_int ~p = [|(1, p); (0, 1. -. p)|]

let bernoulli ~p = [|(true, p); (false, 1. -. p)|]

let to_string print_val dist =
  dist
  |> Array.map (fun (v, score) ->
         Printf.sprintf "S(%s) = %f" (print_val v) score )
  |> Array.to_list |> String.concat ", "

let draw d =
  let n = Array.length d in
  assert (n > 0) ;
  let rec loop i cutoff =
    assert (i >= 0) ;
    let v, prob = d.(i) in
    if cutoff <= 0. then v else loop (i - 1) (cutoff -. prob)
  in
  loop (n - 1) (Random.float 1.)

let prob d v = Array.assoc_exn ~key:v d

let of_sdist sd =
  let sd = Array.map_snd exp sd in
  let total = Array.fold_left_snd ( +. ) 0. sd in
  Array.map_snd (fun e -> e /. total) sd

let to_sdist = Fun.id