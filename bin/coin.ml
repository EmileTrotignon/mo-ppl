open Utils
open Mo_ppl.Continuous
open Matplotlib

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

let dist =
  Model.infer_metropolis_hasting ~n:10000
    (coin [false; true; true; false; false; false; false; false; false; false])

let () =
  Stdlib.flush_all () ;
  Mpl.style_use "ggplot" ;
  let _fig, ax = Fig.create_with_ax () in
  Ax.scatter ax (dist |> Mo_ppl.Continuous.Dist.support) ;
  Ax.set_title ax "Coin" ;
  Ax.set_xlabel ax "Sample values" ;
  Ax.set_ylabel ax "Probability density" ;
  Mpl.show ()
