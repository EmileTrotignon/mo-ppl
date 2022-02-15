open Mo_ppl.Finite

(* should be impossible to do that *)
let funny_bernoulli_ugly =
  Model.(
    Sample
      ( sample (Dist.bernoulli_int ~p:0.5)
      , fun a ->
          Sample
            ( sample (Dist.bernoulli_int ~p:0.5)
            , fun b ->
                Sample
                  ( sample (Dist.bernoulli_int ~p:0.5)
                  , fun (c : int) -> Assume (a = 1 || b = 1, Return (a + b + c))
                  ) ) ))

let funny_bernoulli =
  (* There needs to be a way to force the use of [sample], it is not required
     for now. A solution would be to have a type [sample] different from type
     [dist] *)
  Model.(
    let* a = sample (Dist.bernoulli_int ~p:0.5) in
    let* b = sample (Dist.bernoulli_int ~p:0.5) in
    let* c = sample (Dist.bernoulli_int ~p:0.5) in
    assume (a = 1 || b = 1) (return (a + b + c)))

let d = infer funny_bernoulli

let () = print_endline @@ Dist.to_string string_of_int d