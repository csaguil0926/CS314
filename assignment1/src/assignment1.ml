(********************)
(* Problem 1: range *)
(********************)

let rec range num1 num2 =
  if num2 < num1 then 
    []
  else 
    num1 :: (range(num1+1) num2)
    
(**********************)
(* Problem 2: flatten *)
(**********************)

let rec flatten l =
  match l with
  [] -> []
  | (h::t) -> h @ flatten(t)

(*****************************)
(* Problem 3: remove_stutter *)
(*****************************)

let rec remove_stutter l =
  match l with
  [] -> []
  | (h::[]) -> [h]
  | (h::x::t) -> if h=x then remove_stutter(x::t) else h::remove_stutter(x::t)

(*******************)
(* Problem 4: sets *)
(*******************)

let rec elem x a =
  match a with
  | [] -> false
  | h::t -> if h = x then true else (elem x t)

let rec subset a b =
  match a with
  | [] -> true
  | h::t -> if (elem h b) then (subset t b) else false

let rec eq a b =
  (subset a b) && (subset b a)

let rec remove x a =
  match a with
  [] -> []
  | (h::t) -> if x = h then remove x t else h::remove x t

let union a b =
  match b with
  [] -> a
  | (h::t) -> remove_stutter (a @ b)

let rec diff a b =
  match b with
  [] -> a
  | (h::t) ->  diff (remove h a) t

(*****************************************************)
(* Problem 5: Digital Roots and Additive Persistence *)
(*****************************************************)

(* digitsOfInt : int -> int list
 * we assume n >= 0
 * (digitsOfInt n) is the list of digits of n in the order in which they appear in n
 * e.g. (digitsOfInt 31243) is [3;1;2;4;3]
 *)

let rec loopDigit l n =
  if n = 0 then l
  else loopDigit (n mod 10::l) (n/10)

let digitsOfInt n =
  if n < 0 then []
  else if n = 0 then [0]
  else loopDigit [] n

(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

let rec sum l =
  match l with
  [] -> 0
  | (h::t) -> h + sum t

let rec additivePersistence n =
  if (n<=9) then 0
  else additivePersistence(sum(digitsOfInt(n))) + 1

let rec digitalRoot n =
  if (n<=9) then n
  else digitalRoot(sum(digitsOfInt(n)))

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for range *)
  let _ =
    try
      assert (range 2 5 = [2;3;4;5]);
      assert (range 0 0 = [0])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for flatten *)
  let _ =
    try
      assert (flatten [[1;2];[3;4]] = [1;2;3;4]);
      assert (flatten [[1;2];[];[3;4];[]] = [1;2;3;4])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove_stutter *)
  let _ =
    try
      assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]);
      assert (remove_stutter [] = []);
      assert (remove_stutter [1;1;1;1;1] = [1]);
      assert (remove_stutter [1;1;1;1;1;2] = [1;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of elem *)
  let _ =
    try
      assert (elem 3 [] = false);
      assert (elem 5 [2;3;5;7;9] = true);
      assert (elem 4 [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of subset *)
  let _ =
    try
      assert (subset [5] [2;3;5;7;9] = true);
      assert (subset [5;3] [2;3;5;7;9] = true);
      assert (subset [5;4] [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of eq *)
  let _ =
    try
      assert (eq [5;3;2;9;7] [2;3;5;7;9] = true);
      assert (eq [2;3;7;9] [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove *)
  let _ =
    try
      assert (eq (remove 5 []) []);
      assert (eq (remove 5 [2;3;5;7;9]) [2;3;9;7]);
      assert (eq (remove 4 [2;3;5;7;9]) [2;3;5;9;7]);
      assert (eq (remove 9 [2;3;5;7;9]) [2;5;3;7]);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for union *)
  let _ =
    try
      assert (eq (union [2;3;5] []) [2;3;5]);
      assert (eq (union [5;2] [3;7;9]) [2;3;5;9;7]);
      assert (eq (union [2;3;9] [2;7;9]) [2;3;9;7]);
      assert (eq (union [] [2;7;9]) [2;9;7])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for diff *)
  let _ =
    try
      assert (eq (diff [1;3;2] [2;3]) [1]);
      assert (eq (diff ['a';'b';'c';'d'] ['a';'e';'i';'o';'u']) ['b';'c';'d']);
      assert (eq (diff ["hello";"ocaml"] ["hi";"python"]) ["hello";"ocaml"]);
      assert (eq (diff ["hi";"ocaml"] ["hello";"ocaml"]) ["hi"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitsOfInt *)
  let _ =
    try
      assert (digitsOfInt 3124 = [3;1;2;4]);
      assert (digitsOfInt 352663 = [3;5;2;6;6;3]);
      assert (digitsOfInt 31243 = [3;1;2;4;3]);
      assert (digitsOfInt 23422 = [2;3;4;2;2]);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for additivePersistence *)
  let _ =
    try
      assert (additivePersistence 9876 = 2)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitalRoot *)
  let _ =
    try
      assert (digitalRoot 9876 = 3);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 9 programming questions are incorrect.\n") (!error_count)

let _ = main()
