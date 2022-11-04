let main () =
    (*function definition*)
    let _ = print_int (Util.add 10 20) in (*this prints integer as a string*)
    (*in at end basically means and, so print int and print string*)
    print_string "\n" (*this prints new line*)
let () = main () (*this basically says ok just run it, without this it would just be defining the function*)
(* if i comment out last line then it prints out nothing because above is just function definition*)

(*compiles two files together by using ocamlc util.ml main.ml
* order to compile matters util.ml must be before main.ml*)