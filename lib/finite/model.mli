(** The type for finite models. Models can be run to get a value and a score.
    The score is a mesure of how likely was that value to be picked. By just
    running the model, there is no guarantee the likeliness of picking a value
    is proportionnal to its score : for that you need to infer the model. *)
type 'a t

val sample : 'a Dist.t -> ('a -> 'b t) -> 'b t
(** [sample dist (fun a -> m)] is the model [m] where [a] is subsituted to a
    sample of [dist] *)

val ( let* ) : 'a Dist.t -> ('a -> 'b t) -> 'b t
(** Let-operator alias of [sample]. *)

val assume : bool -> 'a t -> 'a t
(** [assume b m] is the model [m] where [b] needs to be true. *)

val factor : float -> 'a t -> 'a t
(** [factor s m] is the model [m], whose likeliness is multiplied by [s]. *)

val return : 'a -> 'a t
(** [return v] is the model whose value, score pair is [(v, 1.)] *)

val run : 'a t -> 'a * float
(** [run m] is a pair [(v, s)] where [v] is a value, and [s] is proportionnal to
    the likelihood of picking that value according to the model, that is
    according to the distribution [infer m].
    This score is not accurate if there are multiple ways to get that value. *)

val infer : 'a t -> 'a Dist.t
(** [infer m] is the distribution corresponding to [m]. Uses an exhaustive
    enumeration algorithm : beware of the number of samples. Needs the absence
    of side effects in order to be correct. *)