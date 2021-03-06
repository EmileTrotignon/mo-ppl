open Utils

type 'a t = ('a * float) array

let bernoulli_int ~p = [|(1, p); (0, 1. -. p)|]

let bernoulli ~p = [|(true, p); (false, 1. -. p)|]

let dice ~sides = Array.init sides (fun i -> (i + 1, 1. /. float_of_int sides))

let to_string print_val dist =
  dist
  |> Array.map (fun (v, score) ->
         Printf.sprintf "S(%s) = %f" (print_val v) score )
  |> Array.to_list |> String.concat ", "

let draw d =
  let n = Array.length d in
  assert (n > 0) ;
  let rec loop i cutoff =
    (* Printf.printf "I=%i, cutoff=%f\n" i cutoff ; *)
    assert (i >= 0) ;
    let v, prob = d.(i) in
    let cutoff = cutoff -. prob in
    (* Printf.printf "prob=%f\n" prob ; *)
    if cutoff <= 0. then v else loop (i - 1) cutoff
  in
  loop (n - 1) (Random.float 1.)

let prob d v = Option.value ~default:0. (Array.assoc ~key:v d)

let of_sdist sd =
  (* let sd = Array.map_snd exp sd in *)
  let total = Array.fold_left_snd ( +. ) 0. sd in
  printf "Normalizing with total being %f\n" total ;
  assert (total <> 0.) ;
  Array.map_snd (fun e -> e /. total) sd

let to_sdist = Fun.id

let map f dist = Array.map_fst f dist
