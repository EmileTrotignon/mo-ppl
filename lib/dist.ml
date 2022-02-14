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
