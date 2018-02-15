%{ (* Grammar of while programs *)

open Ast

let skip = PTick (0, ())

let mk_while cond bdy =
  let nc = Ast.cond_neg cond in
  PLoop (PSeq (PIf (nc, PBreak (), skip, ()), bdy, ()), ())

%}

%token ASSERT WHILE LOOP IF ELSE BREAK
%token SEMI COMMA PLUS MINUS STAR
%token LPAREN RPAREN
%token LT GT LE GE EQ
%token FUNC LOCAL RETURN
%token <string> IDNT
%token <int> NUM
%token EOF

%left PLUS MINUS
%left STAR

%nonassoc THN
%nonassoc ELSE

%start program
%type <unit Ast.func list * unit Ast.prog> program

%%

program: funcs seq EOF {$1, $2};

names: LPAREN RPAREN {[]} | LPAREN names1 RPAREN {$2};
names1: IDNT {[$1]} | IDNT COMMA names1 {$1::$3};

func: FUNC IDNT names LOCAL names atom {{fname=$2; fargs=$3; flocs=$5; fbody=$6}}
    | FUNC IDNT names atom             {{fname=$2; fargs=$3; flocs=[]; fbody=$4}}
;

funcs: {[]} | func SEMI funcs {$1::$3};

num: NUM {(+$1)} | MINUS NUM {(-$2)};
var: num {VNum $1} | IDNT {VId $1};

lsum: var                  {LVar $1}
    | num STAR lsum        {LMult ($1, $3)}
    | LPAREN lsum RPAREN   {$2}
    | lsum PLUS lsum       {LAdd ($1, $3)}
    | lsum MINUS lsum      {LSub ($1, $3)}
;

cond: STAR         {CNonDet}
    | lsum LE lsum {CTest ($1, CLe, $3)}
    | lsum GE lsum {CTest ($1, CGe, $3)}
    | lsum LT lsum {CTest ($1, CLt, $3)}
    | lsum GT lsum {CTest ($1, CGt, $3)}
;

vars: LPAREN RPAREN {[]} | LPAREN vars1 RPAREN {$2};
vars1: var {[$1]} | var COMMA vars1 {$1::$3};

atom: IDNT PLUS EQ var       {PInc ($1, OPlus, $4, ())}
    | IDNT MINUS EQ var      {PInc ($1, OMinus, $4, ())}
    | IDNT EQ IDNT vars      {PCall (Some $1, $3, $4, ())}
    | IDNT vars              {PCall (None, $1, $2, ())}
    | IDNT EQ var            {PSet ($1, Some $3, ())}
    | IDNT EQ STAR           {PSet ($1, None, ())}
    | LPAREN RPAREN          {skip}
    | LPAREN num RPAREN      {PTick ($2, ())}
    | BREAK                  {PBreak ()}
    | RETURN var             {PReturn ($2, ())}
    | WHILE cond atom        {mk_while $2 $3}
    | LOOP atom              {PLoop ($2, ())}
    | ASSERT cond            {PAssert ($2, ())}
    | IF cond atom %prec THN {PIf ($2, $3, skip, ())}
    | IF cond atom ELSE atom {PIf ($2, $3, $5, ())}
    | LPAREN seq RPAREN      {$2}
;

seq: atom {$1} | atom SEMI seq {PSeq ($1, $3, ())};

%%
