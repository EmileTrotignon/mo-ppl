open Mo_ppl.Finite

let funny_bernoulli =
  Model.(
    let* a = Dist.bernoulli_int ~p:0.5 in
    let* b = Dist.bernoulli_int ~p:0.5 in
    let* c = Dist.bernoulli_int ~p:0.5 in
    assume (a = 1 || b = 1) (return (a + b + c)))

let dist = Model.infer funny_bernoulli

let () = print_endline @@ Dist.to_string string_of_int dist
