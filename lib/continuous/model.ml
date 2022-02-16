open Utils

type 'a t =
  | Return : 'a -> 'a t
  | Assume : (bool * 'a t) -> 'a t
  | Factor : (float * 'a t) -> 'a t
  | Sample : ('a Dist.t * ('a -> 'b t)) -> 'b t

let rec run = function
  | Return v ->
      (v, 1.)
  | Assume (b, model) ->
      if b then run model
      else
        let v, _ = run model in
        (v, 0.)
  | Factor (score, model) ->
      let v, score' = run model in
      (score *. score', v)
  | Sample (d, fm) ->
      run (fm (Dist.sample d))

type 'a restartable =
  {subscore: float; update_score: subscore:float -> float; submodel: 'a t}

let rec run_restartable = function
  | Return v ->
      (v, 1., [])
  | Assume (b, model) ->
      if b then run_restartable model
      else
        let v, _, _ = run_restartable model in
        (v, 0., [])
  | Factor (score, model) ->
      let v, score', ks = run_restartable model in
      (v, score *. score', ks)
  | Sample (d, fm) ->
      let v, varscore = Dist.sample_with_score d in
      let v, subscore, ks = run_restartable (fm v) in
      ( v
      , subscore *. varscore
      , { subscore
        ; update_score= (fun ~subscore -> varscore *. subscore)
        ; submodel= Sample (d, fm) }
        :: ks )

let keep_old_trace score score' =
  Finite.Dist.(draw @@ bernoulli ~p:(min 1. (score /. score')))

let update_trace subscore trace i new_trace =
  let prefix = List.sub trace 0 i in
  List.fold_right_map
    (fun subscore old_restartable ->
      (old_restartable.update_score ~subscore, {old_restartable with subscore})
      )
    subscore (prefix @ new_trace)

let update_scores scores value score = Hashtbl.add scores value score

let infer_metropolis_hasting ~n model =
  let scores = Hashtbl.create 256 in
  let rec aux n trace =
    if n = 0 then ()
    else
      let nk = List.length trace in
      let i = Random.int nk in
      let trace =
        let {subscore; submodel; _} = List.nth trace i in
        let value, new_subscore, new_trace = run_restartable submodel in
        let score, prefix = update_trace subscore trace i new_trace in
        update_scores scores value score ;
        if keep_old_trace subscore new_subscore then trace
        else prefix @ new_trace
      in
      aux (n - 1) trace
  in
  let value, score, trace = run_restartable model in
  update_scores scores value score ;
  aux n trace ;
  Dist.of_scores scores

let infer model =
  match model with
  | Return v ->
      [|(v, Finite.Prob.certain)|]
  | Assume (_b, _model) ->
      failwith "TODO"
  | Factor (_score, _t) ->
      failwith "TODO"
  | Sample (_sd, _fm) ->
      failwith "TODO"
