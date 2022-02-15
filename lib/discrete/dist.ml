type 'a t = ('a * Prob.t) list

let empty = []

let bernoulli ~p = [(1, p); (0, 1. -. p)]

let factor score dist = List.map (fun (v, score') -> (v, score *. score')) dist

let list_of_hashtbl hashtbl =
  Hashtbl.fold (fun key data acc -> (key, data) :: acc) hashtbl []

let combine dist_list =
  let final_dist = Hashtbl.create 256 in
  List.iter
    (fun dist ->
      List.iter
        (fun (value, score) ->
          match Hashtbl.find_opt final_dist value with
          | None ->
              Hashtbl.add final_dist value score
          | Some score' ->
              Hashtbl.replace final_dist value (score +. score') )
        dist )
    dist_list ;
  list_of_hashtbl final_dist

let to_string print_val dist =
  dist
  |> List.map (fun (v, score) ->
         Printf.sprintf "S(%s) = %f" (print_val v) score )
  |> String.concat ", "

let draw d =
  if d = empty then
    raise (Invalid_argument "Sampling empty discrete distribution.")
  else
    let rec aux d cutoff =
      match d with
      | [] ->
          raise
            (Invalid_argument
               "Sampling discrete distribution with weights smaller than 1." )
      | (_, p) :: d when cutoff > 0. ->
          aux d (cutoff -. p)
      | (v, _) :: _ ->
          v
    in
    aux d (Random.float 1.)
