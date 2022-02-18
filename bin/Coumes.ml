(* TODO solve third degree polynomials.
 * a3 is the highest degree and the function returns a list of all the zeros (possibly empty). *)
let third_degree a3 a2 a1 a0 = []

(* Returns the lowest positive value in a list. Returns something negative if there isn't one *)
let rec best_value l = 
        match l with
        | a :: ll -> let b = best_value ll in
                if a > 0. && a < b then a else b
        | [] -> -1.

(* Give the next time the ball will touch the ground.
 * Assumes constant negative acceleration of a *)
let touch_ground y0 vy0 a =
        let c2 = -0.5 *. a in
        let c1 = vy0 in
        let c0 = y0 in
        let zeros = third_degree 0. c2 c1 c0 in
        best_value zeros

(* Gives the next time the ball will touch the plateform *)
let intersect_platform x0 y0 vx0 vy0 xp1 yp1 xp2 yp2 a =
        let a3 = a *. vx0 in
        let a2 = 0.5 *. a *. (2. *. x0 -. xp1 -. xp2) +. 2. *. vy0 *. vx0 in
        let a1 = vy0 *. (2. *. x0 -. xp1 -. xp2) +. vx0 *. (2. *. y0 -. yp1 -. yp2) in
        let a0 = (y0 -. yp1) *. (x0 -. xp2) +. (y0 -. yp2) *. (x0 -. xp1) in
        best_value (third_degree a3 a2 a1 a0)

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

        
