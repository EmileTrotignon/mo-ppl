(** The type of "score distribution". What we call an sdist is a distribution
    that is not normalized. It is suitable for constructing distributions.
    To normalize, call [Dist.of_sdist]. *)
type 'a t = ('a * float) array

val empty : 'a t
(** The empty sdist. *)

val score : ('a ) t -> 'a -> float
(** [score sd v] is the score associated to [v] in [sd]. *)

val factor : float -> 'a t -> 'a t
(** [factor f sd] is the distribution with the same domain as [sd], where every
    value has for score its score in [sd] multiplied by [f]. *)

val combine : 'a t array -> 'a t
(** Combine an array of sdist into one sdist by summing the scores of values
    present in multiple sdist. *)
