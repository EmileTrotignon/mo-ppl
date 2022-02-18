(* Computes the new speed of a ball after it bounced off a platerform *)
let bounce_plateform vx vy xp1 yp1 xp2 yp2 =
        let yp = if xp2 > xp1 then yp2 -. yp1 else yp1 -. yp2 in (* The conditions looks like a typo. It is not. *)
        let xp = if xp2 > xp1 then xp2 -. xp1 else xp1 -. xp2 in
        let length_plateform = sqrt (yp ** 2. +. xp ** 2.) in
        let ypn = yp /. length_plateform in
        let xpn = xp /. length_plateform in
        let vxt = vx *. xpn in
        let vyt = vy *. ypn in
        (vxt -. vx, vyt -. vy)

let bounce_plat_vect speed plat1 plat2 = 
        let vx = Gg.x speed in
        let vy = Gg.y speed in
        let xp1 = Gg.x plat1 in
        let yp1 = Gg.y plat1 in
        let xp2 = Gg.x plat2 in
        let yp2 = Gg.y plat2 in
        match bounce_plateform vx vy xp1 yp1 xp2 yp2 with
        (v1, v2) -> Gg.v v1 v2

let step pos speed accel timestep = 
        let pos = pos + Gg.smul timestep speed in
        let speed = speed + Gg.smul timestep accel in
       (pos, speed)

(* Detects colision with a platform *)
let touch pos plat1 plat2 =
        let vec1 = Gg.sub plat1 pos in
        let vec2 = Gg.sub plat1 plat2 in
        let norma_vec1 = let n = Gg.norm vec1 in Gg.smul (1 /. n) vec1 in
        let norma_vec2 = let n = Gg.norm vec2 in Gg.smul (1 /. n) vec2 in
        let dot_prod = Gg.dot norma_vec1 norma_vec2 in
        let vec3 = Gg.sub plat2 pos in
        dot_prod > 0.9 && Gg.dot vec1 vec3 < 0. (* Detect both a flat angle and that we are "between" the two extremities *)

let touch_ground pos = Gg.y pos < 0.



