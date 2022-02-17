type 'a t


(* -------------------------------------------------------------------------- *)
(** Model builders. *)

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

val factor_list : float list -> 'a t -> 'a t

val factor_while : (unit -> float option) -> 'a t -> 'a t

val observe : 'a -> 'a Dist.t -> 'b t -> 'b t

val observe_list : 'a Dist.t -> 'a list -> 'b t -> 'b t

val sample_list : 'a Dist.t list -> ('a list -> 'b t) -> 'b t

val sample_while : (unit -> 'a Dist.t option) -> ('a list -> 'b t) -> 'b t

(* -------------------------------------------------------------------------- *)
(** Inference *)

val run : 'a t -> 'a * float
(** [run m] is a pair [(v, s)] where [v] is a value, and [s] is proportionnal to
    the likelihood of picking that value according to the model, that is
    according to the distribution [infer m].
    This score is not accurate if there are multiple ways to get that value. *)

val infer_metropolis_hasting :
     n:int
  -> ?step_watcher:(int -> ('a, float) Hashtbl.t -> unit)
  -> 'a t
  -> 'a Dist.t
(** [infer_metropolis_hasting ~n ~step_watcher m] is the disitribution
    associated to [m], computed with [n] steps, calling [step_watcher] at each
    step (useful for understanding the behavior of the inference). *)

