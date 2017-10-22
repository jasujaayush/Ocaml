(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* assoc : 'a * 'b * ('b * 'a) list -> 'a
  Case 1 : If the list l is empty, return the default value d
  Case 2: If the head tuple of list has the key k, return the value v of the tuple. 
          Otherwise call assoc in the reamining list *)

let rec assoc (d,k,l) = 
	match l with 
	| [] -> d
	| (s,v)::t -> if s=k then v else assoc (d,k,t);;

(* fill in the code wherever it says : failwith "to be written" *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if (List.mem h seen) then seen else h::seen in  (* If h is already in seen list don't update seen list.
                                                                      Otherwise, add h to seen list*)
        let rest' = t in                                 (* The remaining elements is the tail list always*)
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* wwhile : ('a -> 'a * bool) * 'a -> 'a  
  call the function(f) on the parameter passed(b) along with it
  if the value returned- (b',c) - has boolean c = true, call wwhile on returned value b'
  otherwise return b' *)
let rec wwhile (f,b) = 
	let (b', c) = f b in
	if c then wwhile (f,b') else b' 

(* fixpoint : ('a -> 'a) * 'a -> 'a 
  Using wwhile to implement this. The function designed (func) makes sure that wwhile keeps running (when executing) 
  until the value returned by func is not same as its parameter*)
let fixpoint (f',b) = wwhile (let func x = let v = (f' x) in (v, not(v=x)) in func ,b)

(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)
let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
