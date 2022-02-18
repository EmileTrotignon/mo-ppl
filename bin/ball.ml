open Gg

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
  let vec1 = V2.sub plat1 pos in
  let vec2 = V2.sub plat1 plat2 in
  let norma_vec1 =
    let n = V2.norm vec1 in
    V2.smul (1. /. n) vec1
  in
  let norma_vec2 =
    let n = V2.norm vec2 in
    V2.smul (1. /. n) vec2
  in
  let dot_prod = V2.dot norma_vec1 norma_vec2 in
  let vec3 = V2.sub plat2 pos in
  dot_prod > 0.9 && V2.dot vec1 vec3 < 0.
(* Detect both a flat angle and that we are "between" the two extremities *)

let update_if_bounce pos speed plat1 plat2 =
  if touch pos plat1 plat2 then bounce_plat_vect speed plat1 plat2 else speed

let touch_ground pos = V2.y pos < 0.

(* plat11 and plat12 are the coordinates of the two borders of the first platform *)
let rec simulate pos speed n accel goal plat11 plat12 plat21 plat22 timestep =
  if n = 0 then 1. /. 0.
  else if touch_ground pos then Float.abs (V2.x pos -. goal)
  else
    let pos, speed = step pos speed accel timestep in
    let speed = update_if_bounce pos speed plat11 plat12 in
    let speed = update_if_bounce pos speed plat21 plat22 in
    simulate pos speed n accel goal plat11 plat12 plat21 plat22 timestep
