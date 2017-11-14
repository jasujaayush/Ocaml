type token =
  | Num of (int)
  | Id of (string)
  | EOF
  | LET
  | REC
  | EQ
  | IN
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | LPAREN
  | RPAREN
  | SEMI
  | COLONCOLON
  | TRUE
  | FALSE
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | LT
  | LE
  | NE
  | LBRAC
  | RBRAC

open Parsing;;
let _ = parse_error;;
# 2 "nanoParse.mly"
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano

let rec consAtTheEnd l e = match l with
  | NilExpr       -> Bin (e, Cons, NilExpr)
  | Bin(h, op, t) -> Bin (h, op,   consAtTheEnd t e)
# 43 "nanoParse.ml"
let yytransl_const = [|
    0 (* EOF *);
  259 (* LET *);
  260 (* REC *);
  261 (* EQ *);
  262 (* IN *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* MUL *);
  266 (* DIV *);
  267 (* AND *);
  268 (* OR *);
  269 (* LPAREN *);
  270 (* RPAREN *);
  271 (* SEMI *);
  272 (* COLONCOLON *);
  273 (* TRUE *);
  274 (* FALSE *);
  275 (* FUN *);
  276 (* ARROW *);
  277 (* IF *);
  278 (* THEN *);
  279 (* ELSE *);
  280 (* LT *);
  281 (* LE *);
  282 (* NE *);
  283 (* LBRAC *);
  284 (* RBRAC *);
    0|]

let yytransl_block = [|
  257 (* Num *);
  258 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\006\000\006\000\006\000\006\000\007\000\007\000\007\000\
\009\000\009\000\009\000\010\000\010\000\011\000\011\000\011\000\
\011\000\011\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\006\000\007\000\004\000\006\000\001\000\003\000\001\000\
\003\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\003\000\002\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\001\000\002\000\001\000\001\000\001\000\001\000\
\001\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\030\000\033\000\000\000\000\000\031\000\032\000\
\000\000\000\000\000\000\037\000\001\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
\000\000\000\000\019\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\000\000\000\000\034\000\000\000\000\000\
\000\000\020\000\000\000\000\000\011\000\015\000\016\000\012\000\
\013\000\014\000\000\000\000\000\018\000\000\000\000\000\000\000\
\000\000\004\000\000\000\036\000\000\000\000\000\000\000\002\000\
\000\000\005\000\003\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\029\000\
\019\000\020\000\021\000"

let yysindex = "\002\000\
\033\255\000\000\000\000\000\000\035\255\033\255\000\000\000\000\
\015\255\033\255\005\255\000\000\000\000\019\255\029\255\004\255\
\000\000\003\255\011\255\069\255\000\000\048\255\053\255\042\255\
\037\255\036\255\000\000\000\000\255\254\067\255\067\255\067\255\
\067\255\067\255\067\255\067\255\067\255\069\255\069\255\067\255\
\069\255\069\255\000\000\033\255\056\255\000\000\033\255\033\255\
\033\255\000\000\029\255\004\255\000\000\000\000\000\000\000\000\
\000\000\000\000\011\255\011\255\000\000\069\255\069\255\058\255\
\033\255\000\000\039\255\000\000\033\255\060\255\033\255\000\000\
\033\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\175\000\160\000\187\000\
\000\000\145\000\073\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\172\000\190\000\000\000\000\000\000\000\000\000\
\000\000\000\000\097\000\121\000\000\000\025\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\000\000\000\000\042\000\043\000\056\000\000\000\000\000\
\230\255\006\000\238\255"

let yytablesize = 474
let yytable = "\024\000\
\027\000\043\000\001\000\026\000\028\000\003\000\004\000\005\000\
\032\000\038\000\039\000\059\000\060\000\049\000\033\000\034\000\
\025\000\006\000\040\000\041\000\042\000\007\000\008\000\009\000\
\025\000\010\000\050\000\035\000\036\000\037\000\030\000\011\000\
\027\000\003\000\004\000\005\000\022\000\064\000\023\000\031\000\
\066\000\067\000\068\000\043\000\043\000\006\000\062\000\063\000\
\026\000\007\000\008\000\009\000\044\000\010\000\045\000\046\000\
\047\000\048\000\070\000\011\000\065\000\071\000\072\000\069\000\
\074\000\073\000\075\000\003\000\004\000\003\000\004\000\051\000\
\024\000\052\000\000\000\000\000\000\000\000\000\000\000\006\000\
\000\000\006\000\000\000\007\000\008\000\007\000\008\000\053\000\
\054\000\055\000\056\000\057\000\058\000\011\000\000\000\061\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\000\000\027\000\027\000\
\027\000\000\000\000\000\000\000\000\000\000\000\027\000\027\000\
\027\000\027\000\027\000\000\000\027\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\000\000\025\000\025\000\
\025\000\000\000\000\000\000\000\000\000\000\000\025\000\025\000\
\025\000\025\000\025\000\000\000\025\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\000\000\026\000\026\000\
\026\000\000\000\000\000\000\000\000\000\000\000\026\000\026\000\
\026\000\026\000\026\000\000\000\026\000\024\000\024\000\024\000\
\024\000\000\000\000\000\024\000\024\000\000\000\024\000\024\000\
\024\000\000\000\000\000\000\000\000\000\000\000\024\000\024\000\
\024\000\024\000\024\000\000\000\024\000\022\000\022\000\022\000\
\022\000\000\000\000\000\022\000\022\000\000\000\022\000\022\000\
\022\000\000\000\000\000\000\000\000\000\000\000\022\000\022\000\
\022\000\022\000\022\000\000\000\022\000\023\000\023\000\023\000\
\023\000\000\000\000\000\023\000\023\000\000\000\023\000\023\000\
\023\000\000\000\000\000\000\000\000\000\000\000\023\000\023\000\
\023\000\023\000\023\000\000\000\023\000\021\000\021\000\000\000\
\000\000\000\000\000\000\021\000\021\000\000\000\021\000\021\000\
\000\000\000\000\000\000\000\000\000\000\008\000\021\000\021\000\
\021\000\021\000\021\000\008\000\021\000\008\000\008\000\000\000\
\000\000\007\000\000\000\000\000\006\000\008\000\008\000\007\000\
\000\000\007\000\007\000\008\000\006\000\006\000\000\000\000\000\
\010\000\007\000\007\000\009\000\006\000\006\000\000\000\007\000\
\010\000\010\000\006\000\009\000\009\000\000\000\000\000\000\000\
\010\000\010\000\000\000\009\000\009\000\000\000\010\000\000\000\
\000\000\009\000"

let yycheck = "\006\000\
\000\000\020\000\001\000\010\000\011\000\001\001\002\001\003\001\
\005\001\007\001\008\001\038\000\039\000\015\001\011\001\012\001\
\002\001\013\001\016\001\009\001\010\001\017\001\018\001\019\001\
\000\000\021\001\028\001\024\001\025\001\026\001\012\001\027\001\
\028\001\001\001\002\001\003\001\002\001\044\000\004\001\011\001\
\047\000\048\000\049\000\062\000\063\000\013\001\041\000\042\000\
\000\000\017\001\018\001\019\001\005\001\021\001\002\001\014\001\
\020\001\022\001\065\000\027\001\005\001\023\001\069\000\006\001\
\071\000\006\001\073\000\001\001\002\001\001\001\002\001\030\000\
\000\000\031\000\255\255\255\255\255\255\255\255\255\255\013\001\
\255\255\013\001\255\255\017\001\018\001\017\001\018\001\032\000\
\033\000\034\000\035\000\036\000\037\000\027\001\255\255\040\000\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\005\001\006\001\007\001\
\008\001\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\005\001\006\001\007\001\
\008\001\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\005\001\006\001\007\001\
\008\001\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\005\001\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\006\001\022\001\023\001\
\024\001\025\001\026\001\012\001\028\001\014\001\015\001\255\255\
\255\255\006\001\255\255\255\255\006\001\022\001\023\001\012\001\
\255\255\014\001\015\001\028\001\014\001\015\001\255\255\255\255\
\006\001\022\001\023\001\006\001\022\001\023\001\255\255\028\001\
\014\001\015\001\028\001\014\001\015\001\255\255\255\255\255\255\
\022\001\023\001\255\255\022\001\023\001\255\255\028\001\255\255\
\255\255\028\001"

let yynames_const = "\
  EOF\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  AND\000\
  OR\000\
  LPAREN\000\
  RPAREN\000\
  SEMI\000\
  COLONCOLON\000\
  TRUE\000\
  FALSE\000\
  FUN\000\
  ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LT\000\
  LE\000\
  NE\000\
  LBRAC\000\
  RBRAC\000\
  "

let yynames_block = "\
  Num\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp8) in
    Obj.repr(
# 29 "nanoParse.mly"
                                       ( _1 )
# 304 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 32 "nanoParse.mly"
                                       ( Let(_2,_4,_6) )
# 313 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 33 "nanoParse.mly"
                                       ( Letrec(_3,_5,_7) )
# 322 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 34 "nanoParse.mly"
                                       ( Fun(_2,_4) )
# 330 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Nano.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 35 "nanoParse.mly"
                                       ( If(_2,_4,_6))
# 339 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 36 "nanoParse.mly"
                                       ( _1 )
# 346 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp7) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp6) in
    Obj.repr(
# 39 "nanoParse.mly"
                                       ( Bin(_1,Or,_3) )
# 354 "nanoParse.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp6) in
    Obj.repr(
# 40 "nanoParse.mly"
                                       ( _1 )
# 361 "nanoParse.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp5) in
    Obj.repr(
# 42 "nanoParse.mly"
                                       ( Bin(_1,And,_3) )
# 369 "nanoParse.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp5) in
    Obj.repr(
# 43 "nanoParse.mly"
                                       ( _1 )
# 376 "nanoParse.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 45 "nanoParse.mly"
                                       ( Bin(_1,Eq,_3) )
# 384 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 46 "nanoParse.mly"
                                       ( Bin(_1,Lt,_3) )
# 392 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 47 "nanoParse.mly"
                                       ( Bin(_1,Le,_3) )
# 400 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 48 "nanoParse.mly"
                                       ( Bin(_1,Ne,_3) )
# 408 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 49 "nanoParse.mly"
                                        ( Bin(_1,And,_3) )
# 416 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 50 "nanoParse.mly"
                                       ( Bin(_1,Or,_3) )
# 424 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 52 "nanoParse.mly"
                                       ( _1 )
# 431 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 54 "nanoParse.mly"
                                         ( Bin(_1,Cons,_3) )
# 439 "nanoParse.ml"
               : 'exp54))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "nanoParse.mly"
                                         ( NilExpr )
# 445 "nanoParse.ml"
               : 'exp54))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expseq) in
    Obj.repr(
# 56 "nanoParse.mly"
                                         ( _2 )
# 452 "nanoParse.ml"
               : 'exp54))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp4) in
    Obj.repr(
# 57 "nanoParse.mly"
                                         ( _1 )
# 459 "nanoParse.ml"
               : 'exp54))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp3) in
    Obj.repr(
# 59 "nanoParse.mly"
                                         ( Bin(_1,Plus,_3) )
# 467 "nanoParse.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp3) in
    Obj.repr(
# 60 "nanoParse.mly"
                                        ( Bin(_1,Minus,_3) )
# 475 "nanoParse.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp3) in
    Obj.repr(
# 62 "nanoParse.mly"
                                       ( _1 )
# 482 "nanoParse.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp2) in
    Obj.repr(
# 64 "nanoParse.mly"
                                       ( Bin(_1,Mul,_3) )
# 490 "nanoParse.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp2) in
    Obj.repr(
# 65 "nanoParse.mly"
                                       ( Bin(_1,Div,_3) )
# 498 "nanoParse.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp2) in
    Obj.repr(
# 67 "nanoParse.mly"
                                       ( _1 )
# 505 "nanoParse.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp2) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp1) in
    Obj.repr(
# 69 "nanoParse.mly"
                                       ( App(_1,_2) )
# 513 "nanoParse.ml"
               : 'exp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp1) in
    Obj.repr(
# 70 "nanoParse.mly"
                                       ( _1 )
# 520 "nanoParse.ml"
               : 'exp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "nanoParse.mly"
                                       ( Const _1 )
# 527 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "nanoParse.mly"
                                       ( True )
# 533 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "nanoParse.mly"
                                       ( False )
# 539 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "nanoParse.mly"
                                       ( Var _1 )
# 546 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Nano.expr) in
    Obj.repr(
# 77 "nanoParse.mly"
                                       ( _2 )
# 553 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 79 "nanoParse.mly"
                                       ( consAtTheEnd NilExpr _1 )
# 560 "nanoParse.ml"
               : 'expseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expseq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 80 "nanoParse.mly"
                                       ( consAtTheEnd _1      _3 )
# 568 "nanoParse.ml"
               : 'expseq))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Nano.expr)
