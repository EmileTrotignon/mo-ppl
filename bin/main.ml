open Utils

module FiniteModels = struct
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
                    , fun (c : int) ->
                        Assume (a = 1 || b = 1, Return (a + b + c)) ) ) ))

  let funny_bernoulli =
    Model.(
      let* a = Dist.bernoulli_int ~p:0.5 in
      let* b = Dist.bernoulli_int ~p:0.5 in
      let* c = Dist.bernoulli_int ~p:0.5 in
      assume (a = 1 || b = 1) (return (a + b + c)))

  let d_funny_bernoulli = infer funny_bernoulli

  let () = print_endline @@ Dist.to_string string_of_int d_funny_bernoulli
end

module ContinuousModels = struct
  open Mo_ppl.Continuous
  open Matplotlib

  let laplace =
    Continuous.(
      Model.(
        let* p = Dist.uniform ~a:0.45 ~b:0.55 in
        observe 241945 (Dist.binomial ~p ~n:493472) (return p)))

  let hmm data =
    Continuous.(
      Model.(
        let rec gen states data =
          match (states, data) with
          | [], y :: data ->
              gen [y] data
          | states, [] ->
              return states
          | pre_x :: _, y :: data ->
              let* x = Dist.gaussian ~mu:pre_x ~sigma:1.0 in
              observe y (Dist.gaussian ~mu:x ~sigma:1.0)
              @@ gen (x :: states) data
        in
        gen [] data))

  let step_watcher i scores =
    Mpl.style_use "ggplot" ;
    let _fig, ax = Fig.create_with_ax () in
    Ax.scatter ax (scores |> Hashtbl.to_array) ;
    Ax.set_title ax (sprintf "Step %i" i) ;
    Mpl.show ()

  let coin data =
    Model.(
      let* z = Dist.uniform ~a:0. ~b:1. in
      observe_list (Dist.bernoulli ~p:z) data (return z))

  (*
  let funny_bernoulli =
    Model.(
      let* a = Dist.bernoulli_int ~p:0.5 in
      let* b = Dist.bernoulli_int ~p:0.5 in
      let* c = Dist.bernoulli_int ~p:0.5 in
      assume (a = 1 || b = 1) (return (a + b + c))) *)

  let d_laplace = Model.infer_metropolis_hasting ~n:10000 laplace

  let data = Owl.Arr.linspace 0. 20. 20 |> Owl.Arr.to_array |> Array.to_list

  let d_hmm = Model.infer_metropolis_hasting ~n:10000 (hmm data)

  let d_coin =
    Model.infer_metropolis_hasting ~n:10000
      (coin
         [false; true; true; false; false; false; false; false; false; false] )
end

(* Example using the object oriented api. *)
open Matplotlib

let graph_int_dist ax d =
  Ax.scatter ax
    ( d |> Mo_ppl.Finite.Dist.to_sdist
    |> Array.map (fun (v, s) -> (Float.of_int v, s)) ) ;
  Ax.set_title ax "hist" ;
  Ax.set_xlabel ax "Sample values" ;
  Ax.set_ylabel ax "Frequency"

let graph_float_dist ax d =
  Ax.scatter ax (d |> Mo_ppl.Continuous.Dist.support) ;
  Ax.set_title ax "hist" ;
  Ax.set_xlabel ax "Sample values" ;
  Ax.set_ylabel ax "Frequency"
(*
let () =
  Mpl.style_use "ggplot" ;
  let fig, ax = Fig.create_with_ax () in
  graph_int_dist ax FiniteModels.d_funny_bernoulli ;
  Fig.suptitle fig "the figure suptitle" ;
  Mpl.show () *)

let () =
  Stdlib.flush_all () ;
  Mpl.style_use "ggplot" ;
  let _fig, ax = Fig.create_with_ax () in
  graph_float_dist ax ContinuousModels.d_coin
  (*|> Mo_ppl.Continuous.Dist.map (fun b -> if b then 1. else 0.)*) ;
  Mpl.show ()
