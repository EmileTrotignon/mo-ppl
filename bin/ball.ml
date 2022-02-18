open Gg
open Utils

(* Computes the new speed of a ball after it bounced off a platerform *)
let bounce_plateform vx vy xp1 yp1 xp2 yp2 =
  let yp = if xp2 > xp1 then yp2 -. yp1 else yp1 -. yp2 in
  (* The conditions looks like a typo. It is not. *)
  let xp = if xp2 > xp1 then xp2 -. xp1 else xp1 -. xp2 in
  let length_plateform = sqrt ((yp ** 2.) +. (xp ** 2.)) in
  let ypn = yp /. length_plateform in
  let xpn = xp /. length_plateform in
  let vxt = vx *. xpn in
  let vyt = vy *. ypn in
  (vxt -. vx, vyt -. vy)

let bounce_plat_vect speed plat1 plat2 =
  let vx = V2.x speed in
  let vy = V2.y speed in
  let xp1 = V2.x plat1 in
  let yp1 = V2.y plat1 in
  let xp2 = V2.x plat2 in
  let yp2 = V2.y plat2 in
  match bounce_plateform vx vy xp1 yp1 xp2 yp2 with v1, v2 -> V2.v v1 v2

let step pos speed accel timestep =
  let pos = V2.(pos + V2.smul timestep speed) in
  let speed = V2.(speed + V2.smul timestep accel) in
  (pos, speed)

(* Detects colision with a platform *)
let touch pos plat1 plat2 =
  let vec1 = V2.(plat1 - pos) in
  let vec2 = V2.(plat1 - plat2) in
  let norma_vec1 =
    let n = V2.norm vec1 in
    V2.smul (1. /. n) vec1
  in
  let norma_vec2 =
    let n = V2.norm vec2 in
    V2.smul (1. /. n) vec2
  in
  let dot_prod = V2.dot norma_vec1 norma_vec2 in
  let vec3 = V2.(plat2 - pos) in
  dot_prod > 0.9 && V2.dot vec1 vec3 < 0.
(* Detect both a flat angle and that we are "between" the two extremities *)

let update_if_bounce pos speed plat1 plat2 =
  if touch pos plat1 plat2 then bounce_plat_vect speed plat1 plat2 else speed

let touch_ground pos = V2.y pos < 0.

let timestep = 0.1

(* plat11 and plat12 are the coordinates of the two borders of the first platform *)
let rec simulate pos speed n accel goal plat11 plat12 plat21 plat22 =
  if n = 0 then Float.infinity
  else if touch_ground pos then Float.abs (V2.x pos -. goal)
  else (
    (* V2.(printf "Pos = (%f , %f)\n" (x pos) (y pos)) ;
    flush stdout ; *)
    let pos, speed = step pos speed accel timestep in
    let speed = update_if_bounce pos speed plat11 plat12 in
    let speed = update_if_bounce pos speed plat21 plat22 in
    simulate pos speed (n - 1) accel goal plat11 plat12 plat21 plat22 )

let pos = V2.v 1. 1.

let speed = V2.v 0. 0.

let accel = V2.v 0.0 (-0.1)

let goal = 0.

let n = 10000

let plat11 = V2.v 1. (0.5)

let plat21 = V2.v (-1.) (0.5)

let simulate plat12 plat22 =
  simulate pos speed n accel goal plat11 plat12 plat21 plat22

open Mo_ppl.Continuous

let a = -1.

let b = 1.

let ball =
  Model.(
    let* plat12_x = Dist.uniform ~a ~b in
    let* plat12_y = Dist.uniform ~a ~b in
    let* plat22_x = Dist.uniform ~a ~b in
    let* plat22_y = Dist.uniform ~a ~b in
    let plat_12 = V2.v plat12_x plat12_y in
    let plat_22 = V2.v plat22_x plat22_y in
    let distance = simulate plat_12 plat_22 in
    (* printf "distance is %f\n" distance ;
    flush stdout ;
    let _ = input_line stdin in *)
    factor (1. /. ((distance *. 10.) +. 0.1)) (return (plat_12, plat_22)))

let dist = Model.infer_metropolis_hasting ~n:10000 ball

open Matplotlib

let () =
  Stdlib.flush_all () ;
  Mpl.style_use "ggplot" ;
  let _fig, ax1, ax2 = Fig.create_with_two_axes `vertical in
  Ax.scatter ax1
    ( dist |> Mo_ppl.Continuous.Dist.support |> Array.map_fst fst
    |> Array.map_fst V2.x ) ;
  Ax.scatter ax2
    ( dist |> Mo_ppl.Continuous.Dist.support |> Array.map_fst fst
    |> Array.map_fst V2.y ) ;
  Ax.set_title ax1 "Plat 1 x" ;
  Ax.set_xlabel ax1 "Sample values" ;
  Ax.set_ylabel ax1 "Probality Density" ;
  Ax.set_title ax2 "Plat 1 y" ;
  Ax.set_xlabel ax2 "Sample values" ;
  Ax.set_ylabel ax2 "Probality Density" ;
  Mpl.show () ;
  Ax.scatter ax1
    ( dist |> Mo_ppl.Continuous.Dist.support |> Array.map_fst snd
    |> Array.map_fst V2.x ) ;
  Ax.scatter ax2
    ( dist |> Mo_ppl.Continuous.Dist.support |> Array.map_fst snd
    |> Array.map_fst V2.y ) ;
  Ax.set_title ax1 "Plat 2 x" ;
  Ax.set_xlabel ax1 "Sample values" ;
  Ax.set_ylabel ax1 "Probality Density" ;
  Ax.set_title ax2 "Plat 2 y" ;
  Ax.set_xlabel ax2 "Sample values" ;
  Ax.set_ylabel ax2 "Probality Density" ;
  Mpl.show ()
