let rec infer model =
  Model.(
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
        Sdist.factor score (infer model))

let infer model = Dist.of_sdist (infer model)

module Prob = Prob
module Dist = Dist
module Model = Model
module Sdist = Sdist

type prob = Prob.t

type 'a dist = 'a Dist.t

type 'a model = 'a Model.t
