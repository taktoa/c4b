(* Slicing and normalizing CIL programs *)
open Cil
open Ast
module E = Errormsg


(* test if [v] must be tracked in the analysis *)
let is_tracked v =
  isIntegralType v.vtype &&
  not (List.mem (Attr ("notrack", [])) (typeAttrs v.vtype))

exception Unsupported

let rec int_of_const = function
  | CInt64 (i, _, _) -> Some (Int64.to_int i)
  | CChr c -> int_of_const (charConstToInt c)
  | _ -> None

let rec transl_sum = function
  | Const c -> LVar (VNum (
      match int_of_const c with
      | Some x -> x | _ -> raise Unsupported
    ))
  | Lval (Var vi, NoOffset) when is_tracked vi ->
    LVar (VId (vi.vname))
  | UnOp (Neg, e, _) ->
    LMult (-1, transl_sum e)
  | BinOp (PlusA, e1, e2, _) ->
    LAdd (transl_sum e1, transl_sum e2)
  | BinOp (MinusA, e1, e2, _) ->
    LSub (transl_sum e1, transl_sum e2)
  | BinOp (Mult, e, Const (CInt64 (i, _, _)), _)
  | BinOp (Mult, Const (CInt64 (i, _, _)), e, _) ->
    LMult (Int64.to_int i, transl_sum e)
  | _ -> raise Unsupported

let linearize =
  let rec add (x,n) = function
    | [] -> [(x, n)]
    | (y, m) :: tl when y = x ->
      (x, m+n) :: tl
    | p :: tl -> p :: add (x,n) tl
  in
  let rec f (m,k) mult = function
    | LVar (VNum n) -> (m, k+n*mult)
    | LVar (VId x) -> (add (x,mult) m, k)
    | LMult (n, l) -> f (m,k) (mult*n) l
    | LAdd (l1, l2) ->
      let m1,k1 = f (m,k) mult l1 in
      let m2,k2 = f (m1,k1) mult l2 in
      m2,k2
    | LSub (l1, l2) ->
      let m1,k1 = f (m,k) mult l1 in
      let m2,k2 = f (m1,k1) (-mult) l2 in
      m2,k2
  in f ([], 0) 1

(* simplify assignments *)
let transl_set v exp =
  try
    let m,k = linearize (transl_sum exp) in
    let cv = if List.mem_assoc v m then List.assoc v m else 0 in

    let rec repeat x n k =
      if n = 0 then k else
      PSeq (x, repeat x (n-1) k, ()) in

    let deltas = List.fold_left
      (fun ds (x,n) ->
        let n = if x = v && n > 0 then n-1 else n in
        let x = if x = v then "%tmp" else x in
        if n >= 0
          then repeat (PInc (v, OPlus, VId x, ())) n ds
          else repeat (PInc (v, OMinus, VId x, ())) (-n) ds
      )
      (if k <> 0
        then PInc (v, OPlus, VNum k, ())
        else PTick (0, ())
      ) m
    in
    let p =
      if cv = 0 then
        match deltas with
        | PSeq (PInc (_, OPlus, v', id1), p, id2) ->
          PSeq (PSet (v, Some v', id1), p, id2)
        | PInc (v, OPlus, VNum n, id) ->
          PSet (v, Some (VNum n), id)
        | _ ->
          PSeq (PSet (v, Some (VNum 0), ()), deltas, ())
      else if cv < 0 then
        PSeq (PSet (v, Some (VNum 0), ()), deltas, ())
      else
        deltas in
    if cv = 1 || cv = 0 then p else
    PSeq (PSet ("%tmp", Some (VId v), ()), p, ())

  with Unsupported -> PSet (v, None, ())

let transl_cond = function
  | BinOp ((Ge | Le | Gt | Lt) as bop, e1, e2, _) ->
    let cop =
      match bop with
      | Ge -> CGe | Le -> CLe
      | Gt -> CGt  | Lt -> CLt
      | _ -> assert false in
    (try CTest (transl_sum e1, cop, transl_sum e2)
    with Unsupported -> CNonDet)
  | _ -> CNonDet


(*
  the following program operations can be
  assigned a cost in the [slice] function
  below
*)
type prog_op =
  | OpExp of exp
  | OpSet | OpTest
  | OpBreak | OpLoop
  | OpCall | OpReturn
  | OpSeq | OpTick of int


(* compile a CIL program into a [Parse.prog] program *)
let slice cost {fileName; globals; _} =

  let check_lvalue loc = function
    | (Var v, NoOffset) when is_tracked v ->
      Some v.vname
    | (Var _, _) ->
      None
    | (_, _) -> (* E.s E.error *) ignore (
        E.warn "%s:%d unsupported lvalue"
          loc.file loc.line
      ); None
  in

  let rec seq a b =
    match a with
    | PSeq (a1, a2, id) ->
      seq a1 (seq a2 b)
    | PTick (n1, _) ->
      begin match b with
      | PTick (n2, id) -> PTick (n1 + n2, id)
      | PSeq (PTick (n2, id), b', id') ->
        PSeq (PTick (n1 + n2, id), b', id')
      | _ -> if n1 = 0 then b else PSeq (a, b, ())
      end
    | _ ->
      begin match b with
      | PTick (0, _) -> a
      | _ -> PSeq (a, b, ())
      end
  in

  let rec slice_list
  : 'a. ('a -> unit prog) -> 'a list -> unit prog = fun f l ->
    List.fold_right
      (fun a b -> seq (f a) b) l (PTick (0, ()))

  and slice_block b = slice_list slice_stmt b.bstmts

  and slice_stmt s =
    let pay_pre c p = if c = 0 then p else PSeq (PTick (c, ()), p, ()) in
    let pay_post c p = if c = 0 then p else seq p (PTick (c, ())) in
    match s.skind with

    | Instr il ->
      let slice_instr = function

        | Set (lv, exp, loc) ->
          let pay = pay_pre (cost OpSet + cost (OpExp exp)) in
          (match check_lvalue loc lv with
          | Some v -> pay (transl_set v exp)
          | None -> pay (PTick (0, ()))
          )

        | Call (None, Lval (Var fassert, NoOffset), [exp], _)
        when fassert.vname = "assert" ->
          PAssert (transl_cond exp, ())

        | Call (Some lv, Lval (Var fnondet, NoOffset), [], loc)
        when fnondet.vname = "nondet" ->
          (match check_lvalue loc lv with
          | Some v -> pay_pre (cost OpSet) (PSet (v, None, ()))
          | None -> E.s (
              E.error "%s:%d unsupported non-deterministic assignment"
                loc.file loc.line
            )
          )

        | Call (None, Lval (Var ftick, NoOffset), [Const c], loc)
        when ftick.vname = "tick" ->
          (match int_of_const c with
          | Some n -> PTick (cost (OpTick n), ())
          | None -> E.s (
              E.error "%s:%d invalid tick call"
                loc.file loc.line
            )
          )

        | Call (lvo, Lval (Var fn, NoOffset), el, loc) ->
          let args = List.fold_right (fun e args ->
            match e with
            | Lval (Var arg, NoOffset) ->
              if is_tracked arg then (VId arg.vname) :: args else args
            | Const c ->
              (match int_of_const c with
              | Some n -> (VNum n) :: args
              | None -> E.s (
                  E.error "%s:%d unsupported constant argument"
                    loc.file loc.line
                )
              )
            | e -> ignore (* E.s *) ( (*
                E.log "%s:%d unsupported function call argument (%a)\n"
                  loc.file loc.line d_exp e
              *) );
              args
            ) el [] in
          (match
            match lvo with
            | Some lv -> check_lvalue loc lv | None -> None
          with
          | Some v ->
            pay_pre (cost OpCall) (PCall (Some v, fn.vname, args, ()))
          | None ->
            pay_pre (cost OpCall) (PCall (None, fn.vname, args, ()))
          )

        | Call (_, _, _, loc)
        | Asm (_, _, _, _, _, loc) -> E.s (
            E.error "%s:%d unsupported instruction"
              loc.file loc.line
          )

      in slice_list slice_instr il

    | Return (ropt, _) ->
      let ret =
        match ropt with
        | Some (Lval (Var rv, NoOffset))
        when is_tracked rv ->
          VId (rv.vname)
        | Some (Const c) ->
          (match int_of_const c with
          | Some i -> VNum i | _ -> VNum 0)
        | _ -> VNum 0 in
      pay_pre (cost OpReturn) (PReturn (ret, ()))

    | Goto (_, loc)
    | ComputedGoto (_, loc) -> E.s (
        E.error "%s:%d unsupported goto"
          loc.file loc.line
      )

    | Break _ ->
      pay_pre (cost OpBreak) (PBreak ())

    | Continue loc -> E.s (
        E.error "%s:%d unsupported continue"
          loc.file loc.line
      )

    | If (exp, b1, b2, loc) ->
      PIf
        ( transl_cond exp
        , slice_block b1
        , slice_block b2
        , ()
        )

    | Switch (_, b, _, _) ->
      let mkif p = PIf (CNonDet, p, PTick (0, ()), ()) in
      let cases, _ =
        List.fold_right (fun s (cases, p) ->
          let s' = seq (slice_stmt s) p in
          if List.exists
               (function
                 | Case _ | Default _ -> true
                 | _ -> false)
               s.labels
          then (seq (mkif s') cases, PTick (0, ()))
          else (cases, s')
        ) b.bstmts (PBreak (), PTick (0, ())) in
      PLoop (cases, ())

    | Loop (b, _, _, _) ->
      let freshen = function
        | PTick (n, _) -> PTick (n, ())
        | PInc (x, o, y, _) -> PInc (x, o, y, ())
        | PSet (x, y, _) -> PSet (x, y, ())
        | PCall (r, f, l, _) -> PCall (r, f, l, ())
        | _ -> assert false in
      let rec norm = function
        | PSeq ((PTick _ | PInc _ | PSet _ (* | PCall _ *)) as l, r, id) ->
          PSeq (freshen l, norm (seq r l), id)
        | x -> PLoop (x, ()) in
      norm (pay_post (cost OpLoop) (slice_block b))

    | Block b -> slice_block b

    | TryFinally (_, _, loc)
    | TryExcept (_, _, _, loc) -> E.s (
        E.error "%s:%d unsupported try"
          loc.file loc.line
      )
  in
  let funcs, fmain =
    let rec funs = function
      | GFun (f, _) :: tl -> f :: funs tl
      | _ :: tl -> funs tl
      | [] -> [] in
    try
      match funs globals with
      | [ f ] -> [], f
      | l ->
        ( List.filter (fun f -> f.svar.vname <> "start") l
        , List.find (fun f -> f.svar.vname = "start") l
        )
    with Not_found -> E.s (
      E.error "%s: no functions to analyze" fileName
    )
  in
  let slice_func f =
    let rec filter_track = function
      | v :: vs ->
        if is_tracked v
        then v.vname :: filter_track vs
        else filter_track vs
      | [] -> [] in
    { fname = f.svar.vname
    ; fargs = filter_track f.sformals
    ; flocs = filter_track f.slocals
    ; fbody = slice_block f.sbody
    } in
  ( List.map slice_func funcs
  , slice_block fmain.sbody
  )


let _ =
  let iscfile f =
    let l = String.length f in
    l >= 2 && f.[l-2] = '.' && f.[l-1] = 'c' in
  let otick, fname =
    match Sys.argv with
    | [| _; "-tick"; f |] when iscfile f -> true, f
    | [| _; f |] when iscfile f -> false, f
    | _ -> false, "" in
  if fname <> "" then
  let file = Frontc.parse fname () in
  let otick = otick ||
    List.exists
      (function GPragma (Attr ("tick", _), _) -> true | _ -> false)
      file.globals in
  let metric =
    if otick
    then (function OpTick n -> n | _ -> 0)
    else (function OpLoop|OpCall -> 1 | _ -> 0) in
  let file = (slice metric file) in
  (**
  print_string "Sliced program:\n";
  Parse.pp_file file;
  print_newline ();
  **)
  let file = Tools.clean_file file in
  let file = Hood.lannot file in
  Printf.printf "Analysis using the %s metric:\n"
    (if otick then "tick" else "back-edge");
  Hood.analyze file
