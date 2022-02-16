let random_coin p = Random.float 1. > p

let binomial ~p ~n =
        let rec temp k = if k = 0 then O else if random_coin p then 1 + (temp (k-1)) else temp (k-1) in
        temp n

let uniform ~a ~b =
        let sample () = (Random.float (b -. a)) +. a in
        let logpdf c = if c < b && c > a then 1. /. (b -. a) else 0. in
        C {sample; logpdf}

let laplace =
        Model.(
                let* p = Continuous.sample (Continuous.Dist.uniform ~a:0. ~b:1.) in
                let* g = Continuous.sample (Continuous.Dist.binomial ~p ~n:493472) in
                Continuous.assume (g = 241945) (return p)
        )

(*
let coin_bias x = 
        let* z = Continuous.sample (Continuous.Dist.uniform ~a:0. ~b:1.) in
*)
