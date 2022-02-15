(* open Utils *)

(* -------------------------------------------------------------------------- *)
type 'a t =
  | D of 'a Discrete.Sdist.t
  | C of {sample: unit -> 'a; logpdf: 'a -> float}

(* -------------------------------------------------------------------------- *)

(* let sample = function D d -> Discrete.Dist.draw d | C {sample; _} -> sample () *)

let logpdf v = function
  | D d ->
      log (Discrete.Sdist.score d v)
  | C {logpdf; _} ->
      logpdf v

let of_discrete d = D d

let uniform ~lo ~hi =
  C
    { sample= (fun () -> Random.float (hi -. lo) +. lo)
    ; logpdf= (fun v -> if v >= lo && v <= hi then 0. else -.infinity) }
