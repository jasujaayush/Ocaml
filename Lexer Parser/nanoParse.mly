%{
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano

let rec consAtTheEnd l e = match l with
  | NilExpr       -> Bin (e, Cons, NilExpr)
  | Bin(h, op, t) -> Bin (h, op,   consAtTheEnd t e)
%}

%token <int> Num
%token <string> Id
%token EOF LET REC EQ IN
%token PLUS MINUS MUL DIV AND OR
%token LPAREN RPAREN
%token SEMI COLONCOLON
%token TRUE FALSE
%token FUN ARROW
%token IF THEN ELSE
%token LT LE NE
%token LBRAC RBRAC
/* ADD MORE TOKEN DECLARATIONS HERE */

%start exp
%type <Nano.expr> exp

%%

exp       : exp8                       { $1 }

/* ADD MORE RULES HERE */
exp8      : LET Id EQ exp IN exp       { Let($2,$4,$6) }
          | LET REC Id EQ exp IN exp   { Letrec($3,$5,$7) }
          | FUN Id ARROW exp           { Fun($2,$4) }     
          | IF exp THEN exp ELSE exp   { If($2,$4,$6)}
          | exp7                       { $1 }


exp7      : exp7 OR exp6               { Bin($1,Or,$3) }
          | exp6                       { $1 }

exp6      : exp6 AND exp5              { Bin($1,And,$3) }
          | exp5                       { $1 }

exp5      : exp5 EQ exp54              { Bin($1,Eq,$3) }
          | exp5 LT exp54              { Bin($1,Lt,$3) }
          | exp5 LE exp54              { Bin($1,Le,$3) }
          | exp5 NE exp54              { Bin($1,Ne,$3) }
          | exp5 AND exp54              { Bin($1,And,$3) }
          | exp5 OR exp54              { Bin($1,Or,$3) }
          /* ADD MORE RULES HERE */
          | exp54                      { $1 }

exp54     : exp4 COLONCOLON exp54        { Bin($1,Cons,$3) }
          | LBRAC RBRAC                  { NilExpr }
          | LBRAC expseq RBRAC           { $2 }
          | exp4                         { $1 }

exp4      : exp4 PLUS exp3               { Bin($1,Plus,$3) }
          | exp4 MINUS exp3             { Bin($1,Minus,$3) }
          /* ADD MORE RULES HERE */
          | exp3                       { $1 }

exp3      : exp3 MUL exp2              { Bin($1,Mul,$3) }
          | exp3 DIV exp2              { Bin($1,Div,$3) }
          /* ADD MORE RULES HERE */
          | exp2                       { $1 }

exp2      : exp2 exp1                  { App($1,$2) }
          | exp1                       { $1 }

/* ADD MORE RULES HERE */
exp1      : Num                        { Const $1 }
          | TRUE                       { True }
          | FALSE                      { False }
          | Id                         { Var $1 }
          | LPAREN exp RPAREN          { $2 }

expseq    : exp                        { consAtTheEnd NilExpr $1 }
          | expseq SEMI exp            { consAtTheEnd $1      $3 }

