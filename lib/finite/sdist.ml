(** Score distributions, in log scale *)

open Utils

type 'a t = ('a * float) array

let empty = [||]

let score d v = Array.assoc_exn ~key:v d

let factor score dist = Array.map (fun (v, score') -> (v, score *. score')) dist

let combine dists =
  let final_dist = Hashtbl.create 256 in
  Array.iter
    (fun dist ->
      Array.iter
        (fun (value, score) ->
          match Hashtbl.find_opt final_dist value with
          | None ->
              Hashtbl.add final_dist value score
          | Some score' ->
              Hashtbl.replace final_dist value (score +. score') )
        dist )
    dists ;
  Hashtbl.to_array final_dist
