(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum: int list -> int
initialize base to 0, for case []
add square of number to accumulator in function f *)
let sqsum xs = 
  let f a x = a+(x*x) in 
  let base = 0 in 
    List.fold_left f base xs

(* pipe: ('a -> 'a) list -> 'a -> 'a
Base function maps the integer to the same value.
Function f, returns a function which applies 'x' to accumulated funcion 'a' applied over a variable *)
let pipe fs= 
  let f a x = (fun t -> x (a t)) in
  let base = (fun x -> x) in
    List.fold_left f base fs

(* sepConcat:  string -> string list -> string*)
let rec sepConcat sep sl = match sl with 
  | [] -> ""  (*if sl is empty, return empty string*)
  | h :: t -> 
      let f a x = a^sep^x in  (*append accumulator with separator and a string*)
      let base = h in (*base case for case if there is onlt one element in the list*)
      let l = t in (*tail of list*)
        List.fold_left f base l

(*stringOfList:  ('a -> string) -> 'a list -> string
  Use function f to convert all list elements to string. 
  Use sepConcat with separator " ;" to convert elements into concatenated string.
  Append with '[' and ']'.
*)
let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(*clone: 'a -> int -> 'a list
  Make a clone helper which returns accumulated value if nothing more is to be accumulated (count <=0).
  Else calls the helper with appended data and reduced count
*)
let rec clone x n = 
  let rec clone_helper acc data count = if count <= 0 then acc else clone_helper (acc@[data]) data (count-1) in
  clone_helper [] x n;;

(*clone: 'a -> int -> 'a list
  Make a pad helper which returns accumulated value if nothing more is to be accumulated (count <=0).
  Else calls the pad helper with appended 0 on top of list and reduced count.
  Which list is to be padded is decided based on the size of both lists.
*)
let rec padZero l1 l2 = 
      let size1 = List.length l1 in
      let size2 = List.length l2 in 
      let rec pad_helper acc count = if count = 0 then acc else pad_helper (0::acc) (count-1) in
      if size1 > size2 then (l1, pad_helper l2 (size1 - size2)) else (pad_helper l1 (size2 - size1), l2)

(*removeZero: int list -> int list
  if head is zero, remove zero from list.
  else return the current list
*)
let rec removeZero l = 
        match l with 
        | 0::t -> removeZero t;
        | _ -> l

(*removeZero: int list -> int list
  1. Append both lists with a 0 to make sure last carry is taken care of.
  2. Combine both the list to form a list of tuples which are to be summed.
  3. Function f: takes a tuple in x, (carry, accumulated_sum) in y,
                  appends the sum with new value and updates the carry.
  4. Base case is (0 carry,  empty sum list)
*)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x =  let (n1, n2) = x in
                 let (carry_old, l) = a in  
                 let sum = (n1+n2+carry_old) mod 10 in 
                 let carry = (n1+n2+carry_old)/10 in
                  (carry, [sum]@l)
                  in
    let base = (0,[]) in
    let args = List.rev (List.combine (List.append [0] l1) (List.append [0] l2) ) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(*mulByDigit: int -> int list -> int list
  Create a list of list which contains l, i number of times.
  Use fold_left with bigAdd to sum the elements in the list
*)
let rec mulByDigit i l = 
      let rec args_helper n acc = if n <= 0 then acc else args_helper (n-1) (l::acc)
      in List.fold_left bigAdd [] (args_helper i [])

(*removeZero: int list -> int list -> int list
  1. Take each digit from l2 starting from right, use mulByDigit to ("modified_l1")*digit.
  2. As we move from one digit to another in l2, append l1 with an extra 0 at the end (multiplication padding)
  3. This list of lists is summed using bigAdd and fold_left
*)
let bigMul l1 l2 = 
  let f a x = bigAdd a x in
  let base = [] in
  let args = fst (List.fold_left (fun (x, appendage) y -> (x@[mulByDigit y (l1@appendage)], (appendage@[0]) )) ([],[]) (List.rev l2)) in
  let (_, res) = (0, List.fold_left f base args) in
    res
