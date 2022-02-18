open Mo_ppl.Continuous
open Utils
(* open Matplotlib *)

type player = Atk | Def

let atk_victories = ref 0

let rec round n_atk n_def dices_atk dices_def =
  if n_atk = 0 || n_def = 0 then
    ( (* printf "VICTORY : Attack has %i soldiers, defence has %i\n" n_atk n_def ;
         flush stdout ; *)
      (* let _ = input_line stdin in *)
      n_atk
    , n_def )
  else
    match (dices_atk, dices_def) with
    | [], _ | _, [] ->
        (n_atk, n_def)
    | da :: dices_atk, dd :: dices_def ->
        if da > dd then round n_atk (n_def - 1) dices_atk dices_def
        else round (n_atk - 1) n_def dices_atk dices_def

let round n_atk n_def dices_atk dices_def =
  let dices_atk = List.sort (fun a b -> -compare a b) dices_atk
  and dices_def = List.sort (fun a b -> -compare a b) dices_def in
  (* printf "Attack has %i soldiers, and its dices are : %s\n" n_atk
       (dices_atk |> List.map string_of_int |> String.concat " ") ;
     printf "Defence has %i soldiers, and its dices are : %s\n" n_def
       (dices_def |> List.map string_of_int |> String.concat " ") ; *)
  let n_atk, n_def = round n_atk n_def dices_atk dices_def in
  (* printf "Attack now has %i soldiers, and defence has %i\n\n" n_atk n_def ; *)
  (n_atk, n_def)

let dice6 = Dist.dice ~sides:6

let n_dices n_soldiers player =
  match player with Atk -> min 3 n_soldiers | Def -> min 2 n_soldiers

let dices_dist n = List.init n (fun _ -> dice6)

let rec battle n_atk n_def =
  Model.(
    (* printf "step : Attack has %i soldiers, defence has %i\n" n_atk n_def ; *)
    flush stdout ;
    if n_atk = 0 then return Def
    else if n_def = 0 then (
      atk_victories := !atk_victories + 1 ;
      return Atk )
    else
      let n_dices_atk = n_dices n_atk Atk and n_dices_def = n_dices n_def Def in
      sample_list (dices_dist n_dices_atk) (fun dices_atk ->
          sample_list (dices_dist n_dices_def) (fun dices_def ->
              let n_atk, n_def = round n_atk n_def dices_atk dices_def in
              battle n_atk n_def ) ))

let rec battle_explicit n_atk n_def =
  Model.(
    (* printf "step : Attack has %i soldiers, defence has %i\n" n_atk n_def ; *)
    flush stdout ;
    if n_atk = 0 then return Def
    else if n_def = 0 then (
      atk_victories := !atk_victories + 1 ;
      return Atk )
    else
      let n_dices_atk = n_dices n_atk Atk and n_dices_def = n_dices n_def Def in
      let continue dices_atk dices_def =
        let n_atk, n_def = round n_atk n_def dices_atk dices_def in
        battle_explicit n_atk n_def
      in
      match (n_dices_atk, n_dices_def) with
      | 1, 1 ->
          let* d1_atk = dice6 in
          let* d1_def = dice6 in
          continue [d1_atk] [d1_def]
      | 1, 2 ->
          let* d1_atk = dice6 in
          let* d1_def = dice6 in
          let* d2_def = dice6 in
          continue [d1_atk] [d1_def; d2_def]
      | 2, 1 ->
          let* d1_atk = dice6 in
          let* d2_atk = dice6 in
          let* d1_def = dice6 in
          continue [d1_atk; d2_atk] [d1_def]
      | 2, 2 ->
          let* d1_atk = dice6 in
          let* d2_atk = dice6 in
          let* d1_def = dice6 in
          let* d2_def = dice6 in
          continue [d1_atk; d2_atk] [d1_def; d2_def]
      | 3, 1 ->
          let* d1_atk = dice6 in
          let* d2_atk = dice6 in
          let* d3_atk = dice6 in
          let* d1_def = dice6 in
          continue [d1_atk; d2_atk; d3_atk] [d1_def]
      | 3, 2 ->
          let* d1_atk = dice6 in
          let* d2_atk = dice6 in
          let* d3_atk = dice6 in
          let* d1_def = dice6 in
          let* d2_def = dice6 in
          continue [d1_atk; d2_atk; d3_atk] [d1_def; d2_def]
      | _ ->
          assert false)

let dist =
  Model.infer_metropolis_hasting ~n:100000 ~shrink:true
    ~ignore_sample_score:true (battle 18 15)

let () =
  printf "Attack won %i times\nChances of attack winning are %f, defence %f.\n"
    !atk_victories (Dist.score Atk dist) (Dist.score Def dist)
(*
let () =
  Stdlib.flush_all () ;
  Mpl.style_use "ggplot" ;
  let _fig, ax = Fig.create_with_ax () in
  (* Ax.scatter ax (dist |> Mo_ppl.Continuous.Dist.support) ; *)
  Ax.hist ax [|Dist.score Atk dist; Dist.score Def dist|] ;
  Ax.set_title ax "Coin" ;
  Ax.set_xlabel ax "Sample values" ;
  Ax.set_ylabel ax "Probability density" ;
  Mpl.show () *)
