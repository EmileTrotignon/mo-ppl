open Utils

(* -------------------------------------------------------------------------- *)
type 'a t =
  | F of 'a Finite.Dist.t
  | C of {sample: unit -> 'a; logpdf: 'a -> float}

let sample = function F d -> Finite.Dist.draw d | C {sample; _} -> sample ()

let _logpdf v = function
  | F d ->
      log (Finite.Dist.prob d v)
  | C {logpdf; _} ->
      logpdf v

let score v = function
  | F d ->
      Finite.Dist.prob d v
  | C {logpdf; _} ->
      exp @@ logpdf v

let of_finite d = F d

let sample_with_score d =
  let v = sample d in
  (v, score v d)

let of_sdist sd =
  match sd with
  | Sdist.F sd ->
      F (Finite.Dist.of_sdist sd)
  | Sdist.(C {sample; logpdf}) ->
      C {sample; logpdf}

let to_sdist d =
  match d with
  | F sd ->
      Sdist.F (Finite.Dist.to_sdist sd)
  | C {sample; logpdf= l} ->
      Sdist.(C {sample; logpdf= l})

let support ?(n = 10000) d =
  match d with
  | F d ->
      Finite.Dist.to_sdist d
  | C _ ->
      Array.init n (fun _ -> sample_with_score d)

let of_scores ?(shrink = false) scores =
  let average li =
    let n = li |> List.length |> float_of_int in
    List.fold_left
      (fun acc score ->
        assert (n <> 0.) ;
        acc +. (score /. n) )
      0. li
  in
  let values = Hashtbl.to_seq_keys scores in
  values
  |> Seq.map (fun value ->
         let scores = Hashtbl.find_all scores value in
         (value, if shrink then average scores else List.hd scores) )
  |> Array.of_seq |> Finite.Dist.of_sdist |> of_finite

(* -------------------------------------------------------------------------- *)
(* Distributions *)

let binomial ~p ~n =
  assert (n >= 0 && 0. <= p && p <= 1.) ;
  let sample () =
    let r = Owl_stats.binomial_rvs ~p ~n in
    r
  in
  let logpdf x =
    let r = Owl_stats.binomial_logpdf x ~p ~n in
    (* printf "In binomial p=%f, n=%i, score of %i is %f\n" p n x r ; *)
    r
  in
  C {sample; logpdf}

let uniform ~a ~b =
  assert (a < b) ;
  let sample () =
    let r = Owl_stats.uniform_rvs ~a ~b in
    r
  in
  let logpdf = Owl_stats.uniform_logpdf ~a ~b in
  C {sample; logpdf}

let beta ~a ~b =
  assert (a > 0. && b > 0.) ;
  let sample () = Owl_stats.beta_rvs ~a ~b in
  let logpdf x = Owl_stats.beta_logpdf x ~a ~b in
  C {sample; logpdf}

let gaussian ~mu ~sigma =
  assert (sigma > 0.) ;
  let sample () = Owl_stats.gaussian_rvs ~mu ~sigma in
  let logpdf x = Owl_stats.gaussian_logpdf x ~mu ~sigma in
  C {sample; logpdf}
