(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* Adding sigmoid operator and root on product operator*)
type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr 
  | Sigmoid  of expr
  | Root     of expr * expr * expr

(* sigmoid(x) = (i/(1 + e(-x)))
    root(a,b,c) = (root(abs(a*b*c)))
*)
let rec exprToString e' = 
  match e' with 
  | VarX                      -> "x"
  | VarY                      -> "y"
  | Sine     e                -> String.concat "" ["sin(pi*"; exprToString e;")"]
  | Cosine   e                -> String.concat "" ["cos(pi*"; exprToString e;")"]
  | Sigmoid  e                -> String.concat "" ["(1/(1 + exp(-("; exprToString e;"))))"]
  | Average  (e1,e2)          -> String.concat "" ["(("; exprToString e1; "+";exprToString e2;")/2)"]
  | Times    (e1,e2)          -> String.concat "" [exprToString e1; "*";exprToString e2]
  | Thresh   (e1, e2, e3, e4) -> String.concat "" ["(";exprToString e1; "<";exprToString e2;"?";exprToString e3;":";exprToString e4;")"]
  | Root     (e1,e2,e3)       -> String.concat "" ["root(abs("; exprToString e1;"*";exprToString e2;"*";exprToString e3;"))"]

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildSigmoid(e)                = Sigmoid(e) (*routing for building sigmoid expression*)
let buildRoot(a,b,c)               = Root(a,b,c) (*routing for building root expression*)

let pi = 4.0 *. atan 1.0

let rec eval (e',x,y) = 
  match e' with 
  | VarX -> x
  | VarY -> y
  | Sine e -> sin(pi*.eval (e, x, y))
  | Cosine e -> cos(pi*.eval (e, x, y))
  | Average (e1,e2) -> ((eval(e1, x, y)+.eval(e2, x, y))/.2.0)
  | Times (e1,e2) -> (eval(e1, x, y)*.eval(e2, x, y))
  | Thresh (e1, e2, e3, e4) -> if eval(e1, x, y) < eval(e2, x, y) then eval(e3, x, y) else eval(e4, x, y) 
  | Sigmoid e -> 1.0/.(1.0 +. exp(-1.0*.(eval(e,x,y))))
  | Root (e1,e2,e3) ->  sqrt (abs_float (eval(e1, x, y)*. eval(e2, x, y)*. eval(e3, x, y)))

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildSigmoid((buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
