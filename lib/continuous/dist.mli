type 'a t

(* -------------------------------------------------------------------------- *)
(* Operations on distributions *)

val sample : 'a t -> 'a

val score : 'a -> 'a t -> float

val sample_with_score : 'a t -> 'a * float

val of_sdist : 'a Sdist.t -> 'a t

val to_sdist : 'a t -> 'a Sdist.t

val support : ?n:int -> 'a t -> 'a Finite.Sdist.t

val of_scores : ?shrink:bool -> ('a, float) Hashtbl.t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

(* -------------------------------------------------------------------------- *)
(* Concrecte distributions. *)

val binomial : p:float -> n:int -> int t

val uniform : a:float -> b:float -> float t

val beta : a:float -> b:float -> float t

val gaussian : mu:float -> sigma:float -> float t

val bernoulli : p:float -> bool t