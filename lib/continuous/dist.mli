type 'a t

val sample : 'a t -> 'a

val logpdf : 'a -> 'a t -> float

val sample_with_score : 'a t -> 'a * float

val of_sdist : 'a Sdist.t -> 'a t

val to_sdist : 'a t -> 'a Sdist.t

val of_scores : ('a, float) Hashtbl.t -> 'a t
