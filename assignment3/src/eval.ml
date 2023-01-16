open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

let rec find x env =
  match env with
  [] -> raise UndefinedVar
  | (y,i)::t -> if x = y then i else find x t

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
  match e with
  Number n -> Int_Val n
  |True -> Bool_Val true
  |False -> Bool_Val false
  |Var x -> find x env
  |Plus (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Int_Val a, Int_Val b -> (Int_Val(a+b))
    |_,_ -> raise TypeError
    )
  |Minus (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Int_Val a, Int_Val b -> (Int_Val(a-b))
    |_,_ -> raise TypeError
    )
  |Times (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Int_Val a, Int_Val b -> (Int_Val(a*b))
    |_,_ -> raise TypeError
    )
  |Div (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Int_Val a, Int_Val b -> if b = 0 then raise DivByZeroError else (Int_Val(a/b))
    |_,_ -> raise TypeError
    )
  |Mod (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Int_Val a, Int_Val b -> if b = 0 then raise DivByZeroError else (Int_Val(a mod b))
    |_,_ -> raise TypeError
    )
  |Or (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Bool_Val a, Bool_Val b -> (Bool_Val(a || b))
    |_,_ -> raise TypeError
    )
  |And (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Bool_Val a, Bool_Val b -> (Bool_Val(a && b))
    |_,_ -> raise TypeError
    )
  |Not e ->
    let v = eval_expr e env in
    (match v with
    Bool_Val a -> (Bool_Val(not a))
    |_ -> raise TypeError
    )
  |Lt (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Int_Val a, Int_Val b -> (Bool_Val(a < b))
    |_,_ -> raise TypeError
    )
  |Leq (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Int_Val a, Int_Val b -> (Bool_Val(a <= b))
    |_,_ -> raise TypeError
    )
  |Eq (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in 
    (match v1, v2 with
    Bool_Val a, Bool_Val b -> (Bool_Val(a = b))
    |Int_Val a, Int_Val b -> (Bool_Val(a = b))
    |_,_ -> raise TypeError
    )
  |Fun (x, e) -> Closure(env, x, e)
  |App (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    Closure(funEnv, x, funBody),_ -> eval_expr funBody ((x, v2)::funEnv)
    |_,_ -> raise TypeError
    )
    

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
    match c with
    Skip -> env
    |Comp (c1, c2) -> 
      let env1 = (eval_command c1 env) in
		  let env2 = (eval_command c2 env1) in 
      env2
    |Declare (dtype, x) ->
      (match dtype with 
      Int_Type -> (x, Int_Val(0))::env
      |Bool_Type -> (x, Bool_Val(false))::env
      |Lambda_Type -> (x, Closure (env, "x", Var "x"))::env
      |_ -> raise TypeError
      )
    |Assg (x, e) ->
      let xVal = find x env in
      let eVal = eval_expr e env in
      (match xVal, eVal with
      Int_Val a, Int_Val b -> (x, eVal)::env
      |Bool_Val a, Bool_Val b -> (x, eVal)::env
      |Closure (a,b,c), Closure (d,e,f) -> (x, eVal)::env
      |_,_ -> raise TypeError
      )
    |Cond (e, c1, c2) ->
      let v = eval_expr e env in
      (match v with
      Bool_Val a -> if a = true then (eval_command c1 env) else (eval_command c2 env)
      |_ -> raise TypeError
      )
    |While (e, c) ->
      let v = eval_expr e env in
      (match v with
      Bool_Val a -> if a = true then (eval_command (While(e, c)) (eval_command c env)) else env
      |_ -> raise TypeError
      )
    |For (e, c) ->
      let v = eval_expr e env in
      (match v with
      Int_Val a -> if a > 0 then (eval_command (For(Minus(Number a, Number 1), c)) (eval_command c env)) else env
      |_ -> raise TypeError
      )
    
      
