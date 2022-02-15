type 'a t

val bernoulli : p:float -> int t

val to_string : ('a -> string) -> 'a t -> string

val draw : 'a t -> 'a

val prob : 'a t -> 'a -> float

val of_sdist : 'a Sdist.t -> 'a t

val to_sdist : 'a t -> 'a Sdist.t
