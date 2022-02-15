let rec infer model =
  Model.(
    match model with
    | Return v ->
        [(v, 1.)]
    | Sample (dist, continuation) ->
        Dist.combine
          (List.map
             (fun (v, score) ->
               if score = 0. then Dist.empty
               else Dist.factor score (infer (continuation v)) )
             dist )
    | Assume (cond, model) ->
        if cond then infer model else Dist.empty
    | Factor (score, model) ->
        Dist.factor score (infer model))

module Prob = Prob
module Dist = Dist
module Model = Model

type prob = Prob.t

type 'a dist = 'a Dist.t

type 'a model = 'a Model.t
