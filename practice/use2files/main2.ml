let main () =
    let _ = print_int (add 10 20) in (*get rid of Util.add*)
    print_string "\n"

(* how to run main using top level:
* # #use "util.ml";; 
* val add : int -> int -> int = <fun> //recognizes as a function
* # #use "main2.ml";;
* val main : unit -> unit = <fun> //recognizes main as function
* # main();; //run main
* 30
* - : unit = () *)