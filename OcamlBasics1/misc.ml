(* CSE 130: Programming Assignment 1
 * misc.ml
 *ssh cs130fez@ieng6-246.ucsd.edu
 *)

(* sumList : int list -> int 
   returns the sum of all the integers in the list.
   for an empty list, sum is 0.
   For a non-empty one, get the head & tail and 
   use recursion to return head + sumList(tail)
*) 
let rec sumList l = 
    match l with 
        | [] -> 0
        | h::t -> h + sumList t

(* digitsOfInt : int -> int list 
   returns all the digits in the number.
   Example: 123 -> [1;2;3], -123 -> [1,2,3]
   Algorithm : Check if the absolute value of number is less than 10. if yes then return the absolute number.
   If absolute value is greater or equal to 10, divide the number by 10 and get digits in the remaining number.
   Append the last digit to the digits obtained from the remaining number.
*)
let rec digitsOfInt n =
  match n/10 with
  | 0 -> [abs n]
  | _ -> digitsOfInt (n/10)@[abs n mod 10];;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
let digits n = digitsOfInt (abs n)


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


(* additivePersistence : int -> int 
Additive Persistence of all the single digit numbers (-9 to 9, both including) with be their value
For every number(n) with absolute value more than or equal to 10, it will be 1 + additivePersistence(sum of digits in n) *)
let rec additivePersistence n =
    if (abs n) < 10 then 0 else 
    let x = sumList (digits n) in 1 + additivePersistence x;;


(* digitalRoot : int -> int 
Digital Root of all the single digit numbers (-9 to 9, both including) with be the number itself.
For every number(n) with absolute value more than or equal to 10, it will be digitalRoot(sum of digits in n) *)
let rec digitalRoot n = 
    let x = sumList (digits n) in 
    match x/10 with 
    | 0 -> x
    | _ -> digitalRoot x;;


(* listReverse : 'a list -> 'a list
For an empty list there is nothing to reverse so return an empty list.
For all non-empty ones, get the head & tail through patter matching, 
reverse the tail and append the head at the end of reversed tail *)
let rec listReverse l =
  match l with 
  | [] -> []
  | h::t -> let r = listReverse t in r@[h];;


(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
*)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0;;


(* palindrome : string -> bool
Palindromes read the same both front and backwards.
Get the list of characters in the string, reverse the list and compare both.
If equal, then the string is indeed a palindrome otherwise not.*)
let palindrome w =
    let stringList = explode w in 
    stringList = listReverse stringList;;
(************** Add Testing Code Here ***************)
