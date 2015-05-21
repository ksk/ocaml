(* GADT exhaustivity equivalent to an undecidable problem, Post-Corresponding-Problem *)

(* Symbols and flags *)
type a and b and ok and empty

(* PCP pairs *)
type (_, 't1,'t2) pair =
  | Empty : (empty, unit, unit) pair
  | Pair1 : (_, 't1,'t2) pair -> (ok, a*'t1, b*(a*(a*'t2))) pair
  | Pair2 : (_, 't1,'t2) pair -> (ok, a*(b*'t1), a*(a*'t2)) pair
  | Pair3 : (_, 't1,'t2) pair -> (ok, b*(b*(a*'t1)), b*(b*'t2)) pair

(* A complete GADT-exhaustivity checker should warn 
   if and only if the PCP has a solution. *)
let f : (ok, 't,'t) pair option -> unit = function None -> ()

(* The PCP above is the case where a solution exists. *)
let _ : (ok, 't,'t) pair = Pair3 (Pair2 (Pair3 (Pair1 Empty)))
