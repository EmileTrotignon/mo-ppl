(* open Utils *)

(* -------------------------------------------------------------------------- *)
type 'a t =
  | F of 'a Finite.Sdist.t
  | C of {sample: unit -> 'a; logpdf: 'a -> float}

(* -------------------------------------------------------------------------- *)

(* let sample = function D d -> Discrete.Dist.draw d | C {sample; _} -> sample () *)

let logpdf v = function
  | F d ->
      log (Finite.Sdist.score d v)
  | C {logpdf; _} ->
      logpdf v

let of_finite d = F d

let uniform ~lo ~hi =
  C
    { sample= (fun () -> Random.float (hi -. lo) +. lo)
    ; logpdf= (fun v -> if v >= lo && v <= hi then 0. else -.infinity) }
