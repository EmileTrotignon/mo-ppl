open Utils

type _ t =
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
      (v, score *. score')
  | Sample (d, fm) ->
      let value, score = Dist.sample_with_score d in
      let value, score' = run (fm value) in
      (value, score *. score')

type 'a restartable = {score_acc: float; submodel: 'a t}

let rec run_restartable ?(ignore_sample_score = false) score_acc = function
  | Return v ->
      (v, score_acc, [])
  | Assume (b, model) ->
      if b then run_restartable score_acc model
      else
        let v, score, _ = run_restartable 0. model in
        assert (score = 0.) ;
        (v, 0., [])
  | Factor (factor, model) ->
      run_restartable (score_acc *. factor) model
  | Sample (d, fm) ->
      let v, varscore = Dist.sample_with_score d in
      let varscore = if ignore_sample_score then 1. else varscore in
      let v, score, trace = run_restartable (score_acc *. varscore) (fm v) in
      (v, score, {score_acc; submodel= Sample (d, fm)} :: trace)

let run_restartable ?ignore_sample_score ?(score_acc = 1.) model =
  let v, score, trace = run_restartable ?ignore_sample_score score_acc model in
  (* if score <> 0. then printf "S(%f) = %f\n" (Obj.magic v) score ; *)
  (v, score, trace)

let keep_old_trace score score' =
  if score = 0. then
    (* printf "Old score was 0, not keeping the old trace\n" ; *)
    false
  else
    let p = min 1. (score' /. score) in
    (* printf "Probability to keep is : %f" p ; *)
    let v = Finite.Dist.(draw @@ bernoulli ~p) in
    (* printf "%s the old trace, probabilty to keep was %f\n"
       (if v then "Keeping" else "Discarding")
       p ; *)
    v

let update_trace trace i new_trace =
  let prefix = List.sub trace 0 i in
  let trace = prefix @ new_trace in
  trace

let update_scores scores value score =
  if score <> 0. then printf "Adding S(%f) = %f\n" (Obj.magic value) score ;
  Hashtbl.add scores value score

let infer_metropolis_hasting ~n ?(step_watcher = fun _i _scores -> ()) model =
  let scores = Hashtbl.create 256 in
  let rec aux n trace score =
    if n = 0 then ()
    else
      let trace_length = List.length trace in
      let i = Random.int trace_length in
      (* printf "Rerunning variable %i\n" i ; *)
      let {score_acc; submodel} = List.nth trace i in
      let value, new_score, new_subtrace =
        run_restartable ~ignore_sample_score:false ~score_acc submodel
      in
      let new_trace = update_trace trace i new_subtrace in
      let new_trace_length = List.length new_trace in
      assert (new_trace_length <> 0) ;
      assert (trace_length <> 0) ;
      let new_score = new_score /. float_of_int new_trace_length in
      update_scores scores value new_score ;
      step_watcher n scores ;
      if keep_old_trace score new_score then aux (n - 1) trace score
      else aux (n - 1) new_trace new_score
  in
  let value, score, trace = run_restartable ~ignore_sample_score:false model in
  update_scores scores value score ;
  aux n trace 0. ;
  Dist.of_scores scores

(* -------------------------------------------------------------------------- *)
(* model constructors *)

let ( let* ) x f = Sample (x, f)

let assume cond prog = Assume (cond, prog)

let factor score prog = Factor (score, prog)

let return v = Return v

let sample x f = Sample (x, f)

let rec factor_list scores prog =
  match scores with
  | [] ->
      prog
  | score :: scores ->
      factor score (factor_list scores prog)

let rec factor_while f prog =
  match f () with
  | None ->
      prog
  | Some score ->
      factor score (factor_while f prog)

let observe dist v prog = factor (Dist.score dist v) prog

let rec observe_list dist data prog =
  match data with
  | [] ->
      prog
  | v :: data ->
      observe v dist (observe_list dist data prog)

let sample_list sli fprog =
  let vli = ref [] in
  let rec aux sli fprog =
    match sli with
    | [] ->
        fprog !vli
    | d :: sli ->
        Sample
          ( d
          , fun x ->
              vli := x :: !vli ;
              aux sli fprog )
  in
  aux sli fprog

let sample_while f fprog =
  let vli = ref [] in
  let rec aux () =
    match f () with
    | None ->
        fprog !vli
    | Some d ->
        Sample
          ( d
          , fun x ->
              vli := x :: !vli ;
              aux () )
  in
  aux ()
