let add x y = x-y

(* if you have 2 function with same name top level will use most recent one loaded in so make sure function names are different
* ex: if you load in util.ml then add 3 4, you get 7 but if you load util2.ml then add 3 4 you get -1*)