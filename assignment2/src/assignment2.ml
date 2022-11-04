open List

let rec count x l =
  match l with
  [] -> 0
  | h::t -> if h = x then (count x t) + 1 else count x t

let count123 l =
  (count 1 l, count 2 l, count 3 l)

let rec n_times (f, n, v) =
  if n <= 0 then v
  else n_times (f, n-1, f v)

let rec find p x l =
  match l with
  [] -> []
  | h::t -> if p h x = true then h::find p x t else find p x t

let rec remove x l =
  match l with
  [] -> []
  | h::t -> if x = h then remove x t else h::remove x t

let rec removelist l1 l2 =
  match l1 with
  [] -> l2
  | h::t -> let x = h in removelist t (remove x l2)

let rec buckets p l = 
  match l with
  [] -> []
  | h::t -> let x = h in (find p x l) :: buckets p (removelist (find p x l) l)

let rec loop n cur prev =
  if n = 0 then cur
  else loop (n-1) prev (cur+prev)

let fib_tailrec n =
  loop n 0 1 

let countfold x l =
  List.fold_left(fun acc ele -> if ele = x then acc + 1 else acc) 0 l

let removefold x l =
  List.fold_left(fun acc ele -> if ele = x then acc else ele::acc) [] l

let assoc_helper acc ele l =
  match acc with
  [] -> (ele, countfold ele l)::acc
  | h::t -> if (ele, countfold ele l) = h then acc else (ele, countfold ele l)::(removefold (ele, countfold ele l) acc)

let assoc_list lst =
  List.fold_left (fun acc ele -> assoc_helper acc ele lst) [] lst

let ap fs args =
  match fs with
  [] -> []
  | h::t -> List.fold_left (fun acc ele -> acc @ (List.map ele args)) [] fs

let max_number l =
  List.fold_left max 0 l
  
let maxl2 lst = 
  match lst with
  [] -> 0
  |a::[] -> 0
  |a::b::c -> 
    let x = max_number lst in
    if countfold x lst > 1 then x + x
    else 
      let lst2 = removefold x lst in
      let y = max_number lst2 in
      x+y 

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l

let rec fold_inorder f acc t =
  match t with
   Leaf -> acc
  | Node (l,x,r) -> let accl = fold_inorder f acc l in fold_inorder f (f accl x) r

let rec height t =
  match t with
  Leaf -> 0
  |Node (l,x,r) -> 1 + max (height l) (height r)

let depth t =
  (height t) - 1

let rec key_at_depth d curr t =
  match t with
  Leaf -> []
  |Node(l,x,r) -> if curr = d then [x] else (key_at_depth d (curr + 1) l) @ (key_at_depth d (curr + 1) r)

let keys_at_depth d t =
  key_at_depth d 0 t

let rec helper d t acc=
  if d = 0 then ((keys_at_depth 0 t)::acc)
  else 
  (helper (d-1) t ((keys_at_depth d t)::acc))

let levelOrder t =
  helper (depth t) t []

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for count123 *)
  let _ =
    try
      assert (count123 [3;4;2;1;3] = (1,1,2));
      assert (count123 [4;4;1;2;1] = (2,1,0))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50);
      assert (n_times((fun x-> (x +. 2.0)), 50, 0.0) = 100.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025);
      assert (fib_tailrec 90 = 2880067194370816120);
      assert (fib_tailrec 0 = 0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let a = [true;false;false;true;false;false;false] in
    let b = [] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
      assert ([(false,5);(true,2)] = List.sort cmp (assoc_list a));
      assert ([] = assoc_list b)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let c = [] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
      assert  ([] = ap fs1 c);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for maxl2 *)  
  let _ =
    try
      assert (maxl2 [1;10;2;100;3;400] = 500)
      ; assert (maxl2 [] = 0)
      ; assert (maxl2 [1000;29;10;5;10000;100000] = 110000)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for levelOrder *)
  let _ =
    try
      assert (levelOrder (construct [3;20;15;23;7;9]) = [[3];[20];[15;23];[7];[9]]);
      assert (levelOrder (construct [41;65;20;11;50;91;29;99;32;72]) = [[41];[20;65];[11;29;50;91];[32;72;99]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 9 programming questions are incorrect.\n") (!error_count)

let _ = main()
