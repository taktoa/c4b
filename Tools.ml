(* Simplify and sanitize programs *)
open Ast

module VSet = struct
  include Set.Make(struct type t = var let compare = compare end)
  let of_list = List.fold_left (fun s x -> add x s) empty
  let union = List.fold_left union empty
end


(* collect all global variables *)
let rec prog_vars p =
  let rec lvars = function
    | LAdd (l1, l2) | LSub (l1, l2) -> VSet.union [lvars l1; lvars l2]
    | LMult (_, l) -> lvars l
    | LVar v -> VSet.of_list [v] in
  let cvars = function
    | CTest (l1, _, l2) -> VSet.union [lvars l1; lvars l2]
    | CNonDet -> VSet.empty in
  match p with
  | PTick _ | PBreak _ -> VSet.empty
  | PAssert (c, _) -> cvars c
  | PReturn (v, _) -> VSet.of_list [v]
  | PSet (id, Some v, _) -> VSet.of_list [VId id; v]
  | PSet (id, None, _) -> VSet.of_list [VId id]
  | PCall (Some id, _, vl, _) -> VSet.of_list (VId id :: vl)
  | PCall (None, _, vl, _) -> VSet.of_list vl
  | PInc (id, _, v, _) -> VSet.of_list [VId id; v]
  | PSeq (p1, p2, _) -> VSet.union [prog_vars p1; prog_vars p2]
  | PLoop (p, _) -> prog_vars p
  | PIf (c, p1, p2, _) -> VSet.union [cvars c; prog_vars p1; prog_vars p2]

let func_globals {fbody; flocs; fargs} =
  VSet.diff (prog_vars fbody)
    (VSet.of_list (List.map (fun i -> VId i) (flocs @ fargs)))

let file_globals (fdefs, p) =
  List.fold_left
    (fun s f -> VSet.union [s; func_globals f])
    (prog_vars p) fdefs


(* alpha conversion, give each variable a unique name *)
let alpha_prog =
  let assoc e x = try List.assoc x e with Not_found -> x in
  let trvar e = function
    | VId x -> VId (assoc e x)
    | VNum _ as v -> v in
  let rec trlsum e = function
    | LAdd (l1, l2) -> LAdd (trlsum e l1, trlsum e l2)
    | LSub (l1, l2) -> LSub (trlsum e l1, trlsum e l2)
    | LMult (k, l) -> LMult (k, trlsum e l)
    | LVar v -> LVar (trvar e v) in
  let trcond e = function
    | CTest (l1, c, l2) -> CTest (trlsum e l1, c, trlsum e l2)
    | CNonDet -> CNonDet in
  let rec trprog e = function
    | (PTick _ | PBreak _) as p -> p
    | PAssert (c, id) ->
      PAssert (trcond e c, id)
    | PReturn (v, id) ->
      PReturn (trvar e v, id)
    | PCall (Some x, f, vl, id) ->
      PCall (Some (assoc e x), f, List.map (trvar e) vl, id)
    | PCall (None, f, vl, id) ->
      PCall (None, f, List.map (trvar e) vl, id)
    | PInc (x, op, v, id) ->
      PInc (assoc e x, op, trvar e v, id)
    | PSet (x, Some v, id) ->
      PSet (assoc e x, Some (trvar e v), id)
    | PSet (x, None, id) ->
      PSet (assoc e x, None, id)
    | PLoop (p, id) ->
      PLoop (trprog e p, id)
    | PIf (c, p1, p2, id) ->
      PIf (trcond e c, trprog e p1, trprog e p2, id)
    | PSeq (p1, p2, id) ->
      PSeq (trprog e p1, trprog e p2, id)
  in trprog

let alpha_func olde f =
  let g (l, e) v =
    try
      let v' = List.assoc v (e @ olde) ^ "'" in
      (v'::l, (v,v')::e)
    with Not_found -> (v::l, (v,v)::e) in
  let fargs, e = List.fold_left g ([], []) f.fargs in
  let flocs, e = List.fold_left g ([], e) f.flocs in
  let flocs, fargs = List.rev flocs, List.rev fargs in
  let fbody = alpha_prog e f.fbody in
  ({f with fbody; flocs; fargs}, e @ olde)

let alpha_file ((fdefs, p) as f) =
  let gvars = VSet.fold (fun v g ->
      match v with
      | VId id -> id :: g
      | _  -> g
    ) (file_globals f) [] in
  let e = List.combine gvars gvars in
  let g (fdefs, e) f =
    let (f, e) = alpha_func e f in
    (f::fdefs, e) in
  let fdefs, _ = List.fold_left g ([], e) fdefs in
  (List.rev fdefs, p)


(* sanity checks *)
exception InvalidProgram of string

let invalid s = raise (InvalidProgram s)

let check_funcs (fdefs, p) =
  let arity =
    List.fold_left (fun a {fname; fargs; _} ->
      if List.mem_assoc fname a then
        invalid ("function " ^ fname ^ " is defined twice")
      else
        (fname, List.length fargs) :: a
    ) [] fdefs in
  let rec ckprog = function
    | PTick _ | PBreak _ | PAssert _ | PReturn _
    | PSet _ | PInc _ -> ()
    | PCall (_, f, args, _) ->
      if not (List.mem_assoc f arity) then
        invalid ("function " ^ f ^ " is called but never defined")
      else if List.length args <> List.assoc f arity then
        invalid ("function " ^ f ^ " is called with wrong arity")
      else
        ()
    | PLoop (p, _) -> ckprog p
    | PSeq (p1, p2, _) | PIf (_, p1, p2, _) ->
      ckprog p1; ckprog p2 in
  List.iter (fun f -> ckprog f.fbody) fdefs; ckprog p

let check_control (fdefs, p) =
  let rec ckprog = function
    | PTick _ | PAssert _ | PSet _ | PInc _ | PCall _
    | PReturn _ -> ()
    | PBreak _ -> invalid ("break cannot occur out of a loop")
    | PLoop (p, _) -> ()
    | PSeq (p1, p2, _) | PIf (_, p1, p2, _) ->
      ckprog p1; ckprog p2 in
  List.iter (fun f -> ckprog f.fbody) fdefs; ckprog p

(* all the above tests *)
let check f =
  begin
    check_funcs f;
    check_control f;
  end


(* add tick on loops and function calls *)
let auto_tick (fdefs, p) =
  let rec f = function
    | PLoop (p, _) -> PLoop (Mk.seq (f p) (PTick (1, ())), ())
    | PCall (_, _, _, _) as x -> Mk.seq (PTick (1, ())) x
    | PIf (c, p1, p2, _) -> PIf (c, f p1, f p2, ())
    | PSeq (p1, p2, _) -> Mk.seq (f p1) (f p2)
    | x -> x in
  (List.map (fun d -> {d with fbody=f d.fbody}) fdefs, f p)


(* sanitize a file *)
let clean_file f =
  let () = check f in
  alpha_file f


(* tests *)
let _ =
  let open Parse in
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "-tclean" then
  pp_file (clean_file (pa_file stdin))
