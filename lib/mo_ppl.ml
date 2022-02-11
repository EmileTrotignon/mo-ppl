type prob = float

(* -------------------------------------------------------------------------- *)
(* distributions *)

type 'a dist = ('a * prob) list

let empty_dist = []

let bernoulli ~p = [(1, p); (0, 1. -. p)]

let sample = Fun.id

let print_dist print_val dist =
  List.iter
    (fun (v, score) -> Printf.printf "S(%s) = %f\n" (print_val v) score)
    dist

(* -------------------------------------------------------------------------- *)
(* programs *)

type 'a program =
  | Return : 'a -> 'a program
  | Assume : (bool * 'a program) -> 'a program
  | Factor : (prob * 'a program) -> 'a program
  | Sample : ('a dist * ('a -> 'b program)) -> 'b program

let factor_dist score dist =
  List.map (fun (v, score') -> (v, score *. score')) dist

let list_of_hashtbl hashtbl =
  Hashtbl.fold (fun key data acc -> (key, data) :: acc) hashtbl []

let combine_dist dist_list =
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

let rec infer program =
  match program with
  | Return v ->
      [(v, 1.)]
  | Sample (dist, continuation) ->
      combine_dist
        (List.map
           (fun (v, score) ->
             if score = 0. then []
             else factor_dist score (infer (continuation v)) )
           dist )
  | Assume (cond, program) ->
      if cond then infer program else []
  | Factor (score, program) ->
      factor_dist score (infer program)

(* -------------------------------------------------------------------------- *)
(* program constructors *)

let ( let* ) x f = Sample (x, f)

let assume cond prog = Assume (cond, prog)

let factor score prog = Factor (score, prog)

let return v = Return v


