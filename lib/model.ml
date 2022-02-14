type 'a t =
  | Return : 'a -> 'a t
  | Assume : (bool * 'a t) -> 'a t
  | Factor : (Prob.t * 'a t) -> 'a t
  | Sample : ('a Dist.t * ('a -> 'b t)) -> 'b t

(* -------------------------------------------------------------------------- *)
(* model constructors *)

let ( let* ) x f = Sample (x, f)

let assume cond prog = Assume (cond, prog)

let factor score prog = Factor (score, prog)

let return v = Return v

let sample = Fun.id