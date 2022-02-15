(* -------------------------------------------------------------------------- *)
type 'a t =
  | F of 'a Finite.Dist.t
  | C of {sample: unit -> 'a; logpdf: 'a -> float}

let sample = function F d -> Finite.Dist.draw d | C {sample; _} -> sample ()

let logpdf v = function
  | F d ->
      log (Finite.Dist.prob d v)
  | C {logpdf; _} ->
      logpdf v

let of_finite d = F d

let sample_with_score d =
  let v = sample d in
  (v, logpdf v d)

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

let of_scores scores =
  let average li =
    let n = li |> List.length |> float_of_int in
    List.fold_left (fun acc score -> acc +. (score /. n)) 0. li
  in
  let values = Hashtbl.to_seq_keys scores in
  values
  |> Seq.map (fun value -> (value, average (Hashtbl.find_all scores value)))
  |> Array.of_seq |> Finite.Dist.of_sdist |> of_finite
