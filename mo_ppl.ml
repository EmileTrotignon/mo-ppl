type prob = float

type 'a dist = ('a * prob) list

let bernoulli ~p = [(1, p); (0, 1. -. p)]

let sample = Fun.id

let magic = Obj.magic

type ('a, 'b) program =
  | Return of 'b
  | Assume of (bool * ('a, 'b) program)
  | Sample of ('a dist * ('a -> ('a, 'b) program))

let funny_bernoulli () =
  Sample
    ( sample (bernoulli ~p:0.5)
    , fun a ->
        Sample
          ( sample (bernoulli ~p:0.5)
          , fun b ->
              Sample
                ( sample (bernoulli ~p:0.5)
                , fun (c : int) -> Assume (a = 1 || b = 1, Return (a + b + c))
                ) ) )

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
           (fun (v, score) -> factor_dist score (infer (continuation v)))
           dist )
  | Assume (cond, program) ->
      if cond then infer program else []

let ( let* ) x f = Sample (x, f)

let assume cond prog = Assume (cond, prog)

let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  assume (a = 2)
  @@ let* b = sample (bernoulli ~p:0.5) in
     assume (b = 2)
     @@ let* c = sample (bernoulli ~p:0.5) in
        assume (a = 1 || b = 1) (Return (a + b + c))
