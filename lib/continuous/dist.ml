type 'a t =
  | D of 'a Discrete.Dist.t
  | C of {sample: unit -> 'a; logpdf: 'a -> float}

let of_discrete d = D d