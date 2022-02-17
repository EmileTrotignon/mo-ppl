open Mo_ppl.Continuous
open Utils
open Matplotlib

let laplace =
  Continuous.(
    Model.(
      let* p = Dist.uniform ~a:0.45 ~b:0.55 in
      observe 241945 (Dist.binomial ~p ~n:493472) (return p)))

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

let dist = Model.infer_metropolis_hasting ~n:10000 laplace

open Matplotlib

let () =
  Stdlib.flush_all () ;
  Mpl.style_use "ggplot" ;
  let _fig, ax = Fig.create_with_ax () in
  Ax.scatter ax (dist |> Mo_ppl.Continuous.Dist.support) ;
  Ax.set_title ax "Laplace" ;
  Ax.set_xlabel ax "Sample values" ;
  Ax.set_ylabel ax "Probality Density" ;
  Mpl.show ()
