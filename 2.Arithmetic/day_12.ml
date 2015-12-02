(* 20/02/2015 *)

(************************************************************************************)
(* 28. Determine whether a given integer number is prime. (medium) *)
(************************************************************************************)

let is_prime n = 
  let rec aux x =
    if x < (abs n) then
       (if (abs n) mod x = 0 then false else aux (x+1))
    else true
in aux 2 

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let is_prime_sol n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2;;
(* val is_prime : int -> bool = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Same idea as solution, but you forget about the fact that 1 is NOT a prime 
number at beginning; also solution give shorter run time*)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

let is_prime n = 
  let rec aux x =
    if x < (abs n) then
       (if (abs n) mod x = 0 then false else aux (x+1))
    else true
in aux 2 && n <> 1

(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 29. Determine the greatest common divisor of two positive integer numbers. (medium)
 *)
(************************************************************************************)

let rec gcd x y = 
  if x mod y = 0 then y 
else gcd y (x mod y)

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let rec gcd_sol a b =
    if b = 0 then a else gcd b (a mod b);;
(* val gcd : int -> int -> int = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Good, and solution is wrong*)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
    
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 30. Determine whether two positive integer numbers are coprime. (easy) *)
(************************************************************************************)

let coprime x y = 
  if gcd x y = 1 then true else false

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let coprime_sol a b = gcd a b = 1

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Unecessary true and false keywords *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
    
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)




