type 'a t =
  | Return : 'a -> 'a t
  | Assume : (bool * 'a t) -> 'a t
  | Factor : (float * 'a t) -> 'a t
  | Sample : ('a Sdist.t * ('a -> 'b t)) -> 'b t

