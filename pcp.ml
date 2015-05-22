(* GADT exhaustivity equivalent to an undecidable problem, Post-Corresponding-Problem *)

(* Symbols and flags *)
type a and b and ok and empty

(* PCP pairs *)
type (_, _, _) pair =
  | Empty : (empty, unit, unit) pair
  | Pair1 : (_, 'seq1,'seq2) pair -> (ok,   'seq1*a      , (('seq2*b)*a)*a) pair
  | Pair2 : (_, 'seq1,'seq2) pair -> (ok,  ('seq1*a)*b   ,  ('seq2*a)*a   ) pair
  | Pair3 : (_, 'seq1,'seq2) pair -> (ok, (('seq1*b)*b)*a,  ('seq2*b)*b   ) pair

(* A complete GADT-exhaustivity checker should warn 
   if and only if the PCP has a solution. *)
let f : (ok, 't,'t) pair option -> unit = function None -> ()

(* The PCP above is the case where a solution exists. *)
let x : (ok, 't,'t) pair = Pair1 (Pair3 (Pair2 (Pair3 Empty)))
