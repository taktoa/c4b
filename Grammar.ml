type token =
  | ASSERT
  | WHILE
  | LOOP
  | IF
  | ELSE
  | BREAK
  | SEMI
  | COMMA
  | PLUS
  | MINUS
  | STAR
  | LPAREN
  | RPAREN
  | LT
  | GT
  | LE
  | GE
  | EQ
  | FUNC
  | LOCAL
  | RETURN
  | IDNT of (string)
  | NUM of (int)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "Grammar.mly"
 (* Grammar of while programs *)

open Ast

let skip = PTick (0, ())

let mk_while cond bdy =
  let nc = Ast.cond_neg cond in
  PLoop (PSeq (PIf (nc, PBreak (), skip, ()), bdy, ()), ())

# 41 "Grammar.ml"
let yytransl_const = [|
  257 (* ASSERT *);
  258 (* WHILE *);
  259 (* LOOP *);
  260 (* IF *);
  261 (* ELSE *);
  262 (* BREAK *);
  263 (* SEMI *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* STAR *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LT *);
  271 (* GT *);
  272 (* LE *);
  273 (* GE *);
  274 (* EQ *);
  275 (* FUNC *);
  276 (* LOCAL *);
  277 (* RETURN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  278 (* IDNT *);
  279 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\004\000\005\000\005\000\006\000\006\000\002\000\
\002\000\008\000\008\000\009\000\009\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\012\000\
\012\000\013\000\013\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\003\000\003\000\000\000"

let yylen = "\002\000\
\003\000\002\000\003\000\001\000\003\000\006\000\004\000\000\000\
\003\000\001\000\002\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\001\000\003\000\003\000\003\000\003\000\002\000\
\003\000\001\000\003\000\004\000\004\000\004\000\002\000\003\000\
\003\000\002\000\003\000\001\000\002\000\003\000\002\000\002\000\
\003\000\005\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\013\000\
\010\000\000\000\014\000\000\000\040\000\000\000\039\000\000\000\
\034\000\000\000\000\000\012\000\037\000\000\000\000\000\000\000\
\000\000\031\000\001\000\000\000\009\000\002\000\000\000\000\000\
\000\000\007\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\000\000\000\043\000\035\000\000\000\
\000\000\024\000\000\000\000\000\033\000\000\000\032\000\045\000\
\000\000\003\000\000\000\016\000\015\000\017\000\018\000\000\000\
\000\000\000\000\000\000\000\000\028\000\029\000\000\000\025\000\
\030\000\005\000\006\000\042\000\027\000"

let yydgoto = "\002\000\
\004\000\005\000\016\000\020\000\048\000\006\000\017\000\026\000\
\027\000\028\000\029\000\042\000\068\000"

let yysindex = "\004\000\
\249\254\000\000\247\254\000\000\105\255\012\255\020\255\255\254\
\255\254\105\255\255\254\000\000\080\255\045\255\061\255\035\000\
\030\255\249\254\003\255\093\255\025\255\000\000\005\255\000\000\
\000\000\050\255\000\000\126\255\000\000\105\255\000\000\105\255\
\000\000\043\255\053\255\000\000\000\000\056\255\057\255\037\255\
\041\255\000\000\000\000\105\255\000\000\000\000\069\255\072\255\
\020\255\000\000\000\000\078\255\005\255\005\255\005\255\005\255\
\005\255\005\255\005\255\000\000\084\255\000\000\000\000\045\255\
\045\255\000\000\090\255\087\255\000\000\092\255\000\000\000\000\
\088\255\000\000\105\255\000\000\000\000\000\000\000\000\036\255\
\036\255\036\255\036\255\105\255\000\000\000\000\045\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\117\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\117\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\099\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\065\000\000\000\000\000\000\000\
\000\000\000\000\103\255\000\000\000\000\072\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\036\000\049\000\062\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\104\000\243\255\075\000\052\000\000\000\250\255\245\255\
\249\255\241\255\009\000\058\000\043\000"

let yytablesize = 341
let yytable = "\034\000\
\012\000\035\000\036\000\031\000\001\000\044\000\037\000\052\000\
\021\000\022\000\023\000\003\000\007\000\050\000\021\000\046\000\
\023\000\030\000\018\000\032\000\024\000\025\000\022\000\060\000\
\047\000\061\000\024\000\025\000\036\000\036\000\072\000\019\000\
\067\000\071\000\043\000\023\000\044\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\054\000\055\000\021\000\051\000\
\020\000\066\000\021\000\069\000\036\000\036\000\021\000\062\000\
\085\000\086\000\024\000\025\000\053\000\021\000\070\000\025\000\
\041\000\063\000\024\000\025\000\091\000\038\000\039\000\013\000\
\040\000\064\000\065\000\036\000\073\000\092\000\041\000\067\000\
\008\000\009\000\010\000\011\000\074\000\012\000\054\000\055\000\
\084\000\021\000\076\000\013\000\033\000\008\000\009\000\010\000\
\011\000\087\000\012\000\088\000\014\000\015\000\025\000\040\000\
\013\000\008\000\009\000\010\000\011\000\047\000\012\000\004\000\
\049\000\014\000\015\000\026\000\013\000\008\000\008\000\008\000\
\008\000\045\000\008\000\075\000\090\000\014\000\015\000\089\000\
\008\000\093\000\000\000\000\000\000\000\000\000\054\000\055\000\
\000\000\008\000\008\000\056\000\057\000\058\000\059\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\000\000\012\000\012\000\000\000\012\000\012\000\012\000\012\000\
\012\000\012\000\044\000\000\000\000\000\012\000\012\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\000\000\000\000\
\000\000\000\000\022\000\022\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\022\000\022\000\000\000\000\000\023\000\
\023\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\023\000\023\000\000\000\000\000\020\000\020\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\020\000\020\000\041\000\
\000\000\021\000\021\000\000\000\013\000\041\000\013\000\000\000\
\000\000\000\000\021\000\021\000\013\000"

let yycheck = "\013\000\
\000\000\013\000\014\000\010\000\001\000\000\000\014\000\023\000\
\010\001\011\001\012\001\019\001\022\001\020\000\010\001\013\001\
\012\001\009\000\007\001\011\000\022\001\023\001\000\000\030\000\
\022\001\032\000\022\001\023\001\040\000\041\000\044\000\012\001\
\040\000\041\000\000\000\000\000\007\001\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\009\001\010\001\010\001\023\001\
\000\000\013\001\010\001\011\001\064\000\065\000\010\001\013\001\
\064\000\065\000\022\001\023\001\011\001\000\000\022\001\023\001\
\000\000\013\001\022\001\023\001\075\000\009\001\010\001\000\000\
\012\001\018\001\018\001\087\000\008\001\084\000\018\001\087\000\
\001\001\002\001\003\001\004\001\013\001\006\001\009\001\010\001\
\005\001\010\001\013\001\012\001\013\001\001\001\002\001\003\001\
\004\001\008\001\006\001\013\001\021\001\022\001\023\001\012\001\
\012\001\001\001\002\001\003\001\004\001\022\001\006\001\013\001\
\020\001\021\001\022\001\013\001\012\001\001\001\002\001\003\001\
\004\001\018\000\006\001\049\000\073\000\021\001\022\001\070\000\
\012\001\087\000\255\255\255\255\255\255\255\255\009\001\010\001\
\255\255\021\001\022\001\014\001\015\001\016\001\017\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\013\001\255\255\255\255\021\001\022\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\255\255\255\255\
\255\255\255\255\012\001\013\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\021\001\022\001\255\255\255\255\012\001\
\013\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\021\001\022\001\255\255\255\255\012\001\013\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\021\001\022\001\007\001\
\255\255\012\001\013\001\255\255\005\001\013\001\007\001\255\255\
\255\255\255\255\021\001\022\001\013\001"

let yynames_const = "\
  ASSERT\000\
  WHILE\000\
  LOOP\000\
  IF\000\
  ELSE\000\
  BREAK\000\
  SEMI\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  LPAREN\000\
  RPAREN\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  EQ\000\
  FUNC\000\
  LOCAL\000\
  RETURN\000\
  EOF\000\
  "

let yynames_block = "\
  IDNT\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'funcs) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'seq) in
    Obj.repr(
# 33 "Grammar.mly"
                       (_1, _2)
# 267 "Grammar.ml"
               : unit Ast.func list * unit Ast.prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "Grammar.mly"
                     ([])
# 273 "Grammar.ml"
               : 'names))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'names1) in
    Obj.repr(
# 35 "Grammar.mly"
                                                 (_2)
# 280 "Grammar.ml"
               : 'names))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "Grammar.mly"
             ([_1])
# 287 "Grammar.ml"
               : 'names1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'names1) in
    Obj.repr(
# 36 "Grammar.mly"
                                        (_1::_3)
# 295 "Grammar.ml"
               : 'names1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'names) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 38 "Grammar.mly"
                                       ({fname=_2; fargs=_3; flocs=_5; fbody=_6})
# 305 "Grammar.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 39 "Grammar.mly"
                                       ({fname=_2; fargs=_3; flocs=[]; fbody=_4})
# 314 "Grammar.ml"
               : 'func))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "Grammar.mly"
       ([])
# 320 "Grammar.ml"
               : 'funcs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'func) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'funcs) in
    Obj.repr(
# 42 "Grammar.mly"
                              (_1::_3)
# 328 "Grammar.ml"
               : 'funcs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "Grammar.mly"
         ((+_1))
# 335 "Grammar.ml"
               : 'num))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "Grammar.mly"
                             ((-_2))
# 342 "Grammar.ml"
               : 'num))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'num) in
    Obj.repr(
# 45 "Grammar.mly"
         (VNum _1)
# 349 "Grammar.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "Grammar.mly"
                          (VId _1)
# 356 "Grammar.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 47 "Grammar.mly"
                           (LVar _1)
# 363 "Grammar.ml"
               : 'lsum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'num) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lsum) in
    Obj.repr(
# 48 "Grammar.mly"
                           (LMult (_1, _3))
# 371 "Grammar.ml"
               : 'lsum))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lsum) in
    Obj.repr(
# 49 "Grammar.mly"
                           (_2)
# 378 "Grammar.ml"
               : 'lsum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lsum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lsum) in
    Obj.repr(
# 50 "Grammar.mly"
                           (LAdd (_1, _3))
# 386 "Grammar.ml"
               : 'lsum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lsum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lsum) in
    Obj.repr(
# 51 "Grammar.mly"
                           (LSub (_1, _3))
# 394 "Grammar.ml"
               : 'lsum))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "Grammar.mly"
                   (CNonDet)
# 400 "Grammar.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lsum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lsum) in
    Obj.repr(
# 55 "Grammar.mly"
                   (CTest (_1, CLe, _3))
# 408 "Grammar.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lsum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lsum) in
    Obj.repr(
# 56 "Grammar.mly"
                   (CTest (_1, CGe, _3))
# 416 "Grammar.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lsum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lsum) in
    Obj.repr(
# 57 "Grammar.mly"
                   (CTest (_1, CLt, _3))
# 424 "Grammar.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lsum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lsum) in
    Obj.repr(
# 58 "Grammar.mly"
                   (CTest (_1, CGt, _3))
# 432 "Grammar.ml"
               : 'cond))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "Grammar.mly"
                    ([])
# 438 "Grammar.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vars1) in
    Obj.repr(
# 61 "Grammar.mly"
                                               (_2)
# 445 "Grammar.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 62 "Grammar.mly"
           ([_1])
# 452 "Grammar.ml"
               : 'vars1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vars1) in
    Obj.repr(
# 62 "Grammar.mly"
                                    (_1::_3)
# 460 "Grammar.ml"
               : 'vars1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 64 "Grammar.mly"
                             (PInc (_1, OPlus, _4, ()))
# 468 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 65 "Grammar.mly"
                             (PInc (_1, OMinus, _4, ()))
# 476 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 66 "Grammar.mly"
                             (PCall (Some _1, _3, _4, ()))
# 485 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 67 "Grammar.mly"
                             (PCall (None, _1, _2, ()))
# 493 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 68 "Grammar.mly"
                             (PSet (_1, Some _3, ()))
# 501 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 69 "Grammar.mly"
                             (PSet (_1, None, ()))
# 508 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "Grammar.mly"
                             (skip)
# 514 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'num) in
    Obj.repr(
# 71 "Grammar.mly"
                             (PTick (_2, ()))
# 521 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "Grammar.mly"
                             (PBreak ())
# 527 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 73 "Grammar.mly"
                             (PReturn (_2, ()))
# 534 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 74 "Grammar.mly"
                             (mk_while _2 _3)
# 542 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 75 "Grammar.mly"
                             (PLoop (_2, ()))
# 549 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cond) in
    Obj.repr(
# 76 "Grammar.mly"
                             (PAssert (_2, ()))
# 556 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 77 "Grammar.mly"
                             (PIf (_2, _3, skip, ()))
# 564 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 78 "Grammar.mly"
                             (PIf (_2, _3, _5, ()))
# 573 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'seq) in
    Obj.repr(
# 79 "Grammar.mly"
                             (_2)
# 580 "Grammar.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 82 "Grammar.mly"
          (_1)
# 587 "Grammar.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 82 "Grammar.mly"
                               (PSeq (_1, _3, ()))
# 595 "Grammar.ml"
               : 'seq))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit Ast.func list * unit Ast.prog)
;;
