exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2)

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) =
    let la = listAssoc (x,evn) in
    if la = None then raise (MLFailure ("Variable not bound: "^x)) else
    match la with
    | Some v -> v

let rec eval (evn,e) = 
    match e with 
    | True -> Bool true
    | False -> Bool false
    | Const x -> Int x
    | NilExpr -> Nil
    | Var s -> (lookup (s,evn))
    | If (exp1, exp2, exp3)   -> let Bool cdtn = eval (evn, exp1) in
                                  if cdtn then eval (evn, exp2) else eval (evn, exp3)
    | Bin (exp1,Cons,exp2)-> Pair (eval(evn,exp1), eval(evn,exp2)) 
    | Bin (exp1, Plus, exp2)  -> let Int v1 = eval (evn, exp1) in let Int v2 = eval (evn, exp2) in Int (v1 + v2)
    | Bin (exp1, Minus, exp2) -> let Int v1 = eval (evn, exp1) in let Int v2 = eval (evn, exp2) in Int (v1 - v2)
    | Bin (exp1, Mul, exp2)   -> let Int v1 = eval (evn, exp1) in let Int v2 = eval (evn, exp2) in Int (v1 * v2)
    | Bin (exp1, Div, exp2)   -> let Int v1 = eval (evn, exp1) in let Int v2 = eval (evn, exp2) in Int (v1 / v2)
    | Bin (exp1, Lt, exp2)    -> let Int v1 = eval (evn, exp1) in let Int v2 = eval (evn, exp2) in Bool (v1 < v2)
    | Bin (exp1, Le, exp2)    -> let Int v1 = eval (evn, exp1) in let Int v2 = eval (evn, exp2) in Bool (v1 <= v2)
    | Bin (exp1, And, exp2)   -> let Bool v1 = eval (evn, exp1) in let Bool v2 = eval (evn, exp2) in Bool (v1 && v2)
    | Bin (exp1, Or, exp2)    -> let Bool v1 = eval (evn, exp1) in let Bool v2 = eval (evn, exp2) in Bool (v1 || v2)
    | Bin (exp1, Eq, exp2)    -> (let v1 = eval (evn, exp1) in let v2 = eval (evn, exp2) in 
                                  match (v1, v2) with 
                                  | (Int x, Int y) -> Bool (x = y)
                                  | (Bool x, Bool y) -> Bool (x = y))
    | Bin (exp1, Ne, exp2)    -> (let v1 = eval (evn, exp1) in let v2 = eval (evn, exp2) in 
                                  match (v1, v2) with 
                                  | (Int x, Int y) -> Bool (x != y)
                                  | (Bool x, Bool y) -> Bool (x != y))
    | Let (s, exp2, exp3)     -> let v2 = eval (evn, exp2) in eval ((s,v2)::evn, exp3)
    | Letrec(s, exp2, exp3)   -> (let v2 = eval (evn, exp2) in 
                                 match v2 with
                                 | Closure (e, _, param, expr) -> (eval ((s,Closure (e, Some s, param, expr))::evn, exp3))
                                 | _ -> eval ((s,v2)::evn, exp3))
    | Fun (param, expr)       -> Closure (evn, None, param, expr)
    | App (exp1, exp2)        -> (match exp1 with 
                                 | Var "hd" -> let Pair(p1,p2) = eval(evn,exp2) in p1
                                 | Var "tl" -> let Pair(p1,p2) = eval(evn,exp2) in p2
                                 | _ -> (let Closure (e, fn, param, expr) = eval(evn, exp1) in
                                         let v2 = eval(evn, exp2) in
                                        Printf.printf "here %s\n"(envToString e);
                                        match fn with 
                                        | Some x -> eval([(param,v2)]@evn@e, expr)
                                        | _      -> eval([(param,v2)]@e@evn, expr)))


(********************** Testing Code

  eval ([], Nano.Let ("l", Nano.Bin (Nano.Const 1, Nano.Cons, Nano.NilExpr), 
          Nano.Let ("m", Nano.Bin (Nano.Const 5, Nano.Cons, Nano.NilExpr), 
          Nano.Bin (Nano.App (Nano.Var "hd", Nano.Var "l"), Nano.Cons, Nano.App (Nano.Var "hd", Nano.Var "l")) )));;

  eval ([], Nano.Let ("l", Nano.Bin (Nano.Const 1, Nano.Cons, Nano.NilExpr), 
          Nano.Let ("m", Nano.Bin (Nano.Const 5, Nano.Cons, Nano.NilExpr), 
          Nano.App (Nano.Var "tl", Nano.Var "m") )));;
          
  Printf.printf "%s\n"(envToString e);
  Printf.printf "Function {%s %s} \n" fname (envToString e);
  Printf.printf "current env %s \n"(envToString evn);
  Printf.printf "expr2 %s \n"(valueToString v2);
open Nano;;
eval ([],Fun ("x",Bin(Var "x",Plus,Var "x")));;
eval ([],App(Fun ("x",Bin(Var "x",Plus,Var "x")),Const 3));;
let e3=Let("h",Fun("y",Bin(Var "x", Plus, Var "y")),App(Var "f",Var "h"));;
let e2 = Let("x",Const 100,e3);;
let e1 = Let("f",Fun("g",Let("x",Const 0,App(Var "g",Const 2))),e2);;
eval ([],e1);;
eval ([],Letrec("f",Fun("x",Const 0),Var "f"));;
let e4 = Let("f",Fun ("x",Bin(Var "x",Plus,Var "x")), App(Var "f",Const 2));;

let e5 = Let("f",Fun ("x",Bin(Var "x",Plus,Var "x")), Let("a", Const 10, App (Var "f", Var "a")));;

Let ("f", Fun ("g", Let ("x", Const 0, App (Var "g", Const 2))),
 Let ("x", Const 100,
  Let ("h", Fun ("y", Bin (Var "x", Plus, Var "y")), App (Var "f", Var "h"))))

eval ([],Letrec("fac",Fun("n",If(Bin(Var "n",Eq,Const 0),Const 1,Bin(Var "n",Plus,App(Var "fac",Bin(Var "n",Minus,Const 1))))),App(Var "fac",Const 10)));;

******************************)