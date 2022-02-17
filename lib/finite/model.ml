type 'a t =
  | Return : 'a -> 'a t
  | Assume : (bool * 'a t) -> 'a t
  | Factor : (float * 'a t) -> 'a t
  | Sample : ('a Dist.t * ('a -> 'b t)) -> 'b t

(* -------------------------------------------------------------------------- *)
(* model constructors *)

let sample x f = Sample (x, f)

let ( let* ) = sample

let assume cond prog = Assume (cond, prog)

let factor score prog = Factor (score, prog)

let return v = Return v

let rec run = function
  | Return v ->
      (v, 1.)
  | Sample (d, k) ->
      let v = Dist.draw d in
      run (k v)
  | Assume (b, m) ->
      if b then (fst (run m), 0.) else run m
  | Factor (factor, m) ->
      let v, score = run m in
      (v, score *. factor)

let rec infer model =
  match model with
  | Return v ->
      [|(v, 1.)|]
  | Sample (dist, continuation) ->
      Sdist.combine
        (Array.map
           (fun (v, score) ->
             if score = 0. then Sdist.empty
             else Sdist.factor score (infer (continuation v)) )
           (Dist.to_sdist dist) )
  | Assume (cond, model) ->
      if cond then infer model else Sdist.empty
  | Factor (score, model) ->
      Sdist.factor score (infer model)

let infer model = Dist.of_sdist (infer model)
