{ (* Parsing and printing of while programs *)

open Grammar
exception LexError of char
exception ParseError of string

let kw =
  [ "while", WHILE; "loop", LOOP; "assert", ASSERT; "if", IF
  ; "else", ELSE; "break", BREAK; "func", FUNC; "local", LOCAL
  ; "return", RETURN
  ]

}

rule tok = parse
    [' ' '\t' '\r']             { tok lexbuf }
  | '#' [^ '\n']*               { tok lexbuf }
  | '\n'                        { Lexing.new_line lexbuf; tok lexbuf }
  | ['0'-'9']+ as s             { NUM (int_of_string s) }
  | ['a'-'z' 'A'-'Z' '_']+ as s { try List.assoc s kw with _ -> IDNT s }
  | "<="                        { LE }
  | ">="                        { GE }
  | '<'                         { LT }
  | '>'                         { GT }
  | '='                         { EQ }
  | '*'                         { STAR }
  | '+'                         { PLUS }
  | '-'                         { MINUS }
  | '('                         { LPAREN }
  | ')'                         { RPAREN }
  | ';'                         { SEMI }
  | ','                         { COMMA }
  | eof                         { EOF }

{

open Ast

let pp_var oc = function
  | VNum n -> Printf.fprintf oc "%d" n
  | VId x -> Printf.fprintf oc "%s" x

let pp_file_hooks pre post (fl, prog) =
  let open Printf in

  let rec pp_list p oc = function
    | [x] -> fprintf oc "%a" p x
    | x :: xs -> fprintf oc "%a, %a" p x (pp_list p) xs
    | [] -> () in

  let rec lsum prns = function
    | LAdd (l1, l2) ->
      if prns then printf "(";
      lsum false l1; printf " + "; lsum false l2;
      if prns then printf ")"
    | LSub (l1, l2) ->
      if prns then printf "(";
      lsum false l1; printf " - "; lsum true l2;
      if prns then printf ")"
    | LMult (k, l) ->
      printf "%d * " k;
      lsum true l
    | LVar v ->
      printf "%a" pp_var v in

  let cond = function
    | CTest (l1, cmp, l2) ->
      lsum false l1;
      printf " %s " (
        match cmp with
        | CLe -> "<="
        | CGe -> ">="
        | CLt -> "<"
        | CGt -> ">"
      );
      lsum false l2
    | CNonDet -> printf "*" in

  let rec idnt i =
    if i <> 0 then
      begin print_string " "; idnt (i - 1) end in

  let delta = 2 in

  let rec f lvl prns = function
    | PTick (0, _) -> printf "()"
    | PTick (n, _) -> printf "(%d)" n
    | PBreak _ -> printf "break"
    | PAssert (c, _) -> printf "assert "; cond c
    | PReturn (v, _) -> printf "return %a" pp_var v
    | PCall (xo, f, args, _) ->
      begin match xo with
      | Some x -> printf "%s = " x
      | _ -> ()
      end; printf "%s(%a)" f (pp_list pp_var) args
    | PSet (id, Some v, _) -> printf "%s = %a" id pp_var v
    | PSet (id, None, _) -> printf "%s = *" id
    | PInc (id, o, v, _) ->
      let op = match o with OPlus -> "+" | OMinus -> "-" in
      printf "%s %s= %a" id op pp_var v
    | PSeq (p1,  p2, _) ->
      let lvl' = if prns
        then (idnt lvl; printf "(\n"; lvl + delta)
        else lvl in
      g lvl' true p1; printf ";\n";
      g lvl' false p2;
      if prns then (printf "\n"; idnt lvl; printf ")")
    | PIf (c, p1, p2, _) ->
      printf "if "; cond c; printf "\n";
      g (lvl + delta) true p1;
      begin match p2 with
      | PTick (0, _) -> ()
      | _ ->
        printf "\n"; idnt lvl; printf "else\n";
        g (lvl + delta) true p2
      end
    | PLoop (p, _) ->
      printf "loop\n";
      g (lvl + delta) true p


  and g lvl prns p =
    match p with
    | PSeq (_, _, _) -> f lvl prns p
    | _ ->
      if prns then pre (prog_data p);
      idnt lvl; f lvl prns p;
      if not prns then post (prog_data p)
  in

  let pf {fname; fargs; flocs; fbody} =
    printf "func %s(%a)" fname (pp_list output_string) fargs;
    if flocs <> [] then
      printf " local (%a)\n" (pp_list output_string) flocs
    else
      printf "\n";
    g delta true fbody;
    printf ";\n"
  in

  begin
    List.iter pf fl;
    if fl <> [] then printf "\n";
    g 0 false prog;
    printf "\n";
  end

let pp_file x = let f _ = () in pp_file_hooks f f x
let pp_prog p = pp_file ([], p)

let pa_file ic =
  let open Lexing in
  let lexbuf = from_channel ic in
  try Grammar.program tok lexbuf with
  | Parsing.Parse_error ->
    let e =
      Printf.sprintf "line %d, character %d"
        lexbuf.Lexing.lex_start_p.pos_lnum
        (lexbuf.lex_start_p.pos_cnum -
         lexbuf.lex_start_p.pos_bol + 1) in
    raise (ParseError e)

let _ =
  match try Sys.argv.(1) with _ -> "" with
  | "-tparse" ->
    let p = pa_file stdin in
    pp_file p
  | _ -> ()

}
