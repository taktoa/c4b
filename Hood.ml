(* under the hood *)

open Ast
open Logic
open Tools

(*
	0 - no output
	1 - output final annotation
	2 - output final annotation and constraints
	4 - print sign of variables (ugly)
*)
let debug = 0

(* compute the logical states *)
type pstate = ineq list
type lannot = { lpre: pstate; lpost: pstate }

let lannot =
  let rec f' locs brk lpre prog =
    let f = f' locs in
    let post lpost = {lpre; lpost} in
    match prog with
    | PTick (n, _) -> PTick (n, post lpre)
    | PBreak _ -> brk := lpre :: !brk; PBreak (post bottom)
    | PAssert (c, _) -> PAssert (c, post ((of_cond c) @ lpre))
    | PReturn (v, _) -> PReturn (v, post bottom)
    | PCall (ret, f, args, _) ->
      let locs = match ret with
        | None -> locs
        | Some x -> List.filter ((<>) x) locs in
      PCall (ret, f, args, post (Logic.call locs lpre))
    | PInc (x, op, v, _) -> PInc (x, op, v, post (Logic.incr x op v lpre))
    | PSet (x, vo, id) -> PSet (x, vo, post (Logic.set x vo lpre))
    | PLoop (bdy, _) ->
      let g ps =
        let brk = ref [] in
        let bdy' = f brk ps bdy in
        let post = (prog_data bdy').lpost in
        ((bdy', !brk), post) in
      let (bdy, brk), _ = Logic.fix lpre g in
      PLoop (bdy, post (List.fold_left Logic.merge bottom brk))
    | PIf (c, p1, p2, _) ->
      let p1 = f brk (Logic.conj (of_cond c) lpre) p1 in
      let p2 = f brk (Logic.conj (of_cond (cond_neg c)) lpre) p2 in
      let post1 = (prog_data p1).lpost
      and post2 = (prog_data p2).lpost
      in PIf (c, p1, p2, post (Logic.merge post1 post2))
    | PSeq (p1, p2, _) ->
      let p1 = f brk lpre p1 in
      let p2 = f brk (prog_data p1).lpost p2 in
      PSeq (p1, p2, post (prog_data p2).lpost)
  in let g ({fbody;fargs;flocs;_} as f) =
    { f with fbody = f' (fargs@flocs) (ref []) [] fbody }
  in fun (fl, p) ->
    List.map g fl, f' [] (ref []) [] p


(* indices we use to name lp variables *)
module Idx : sig
  type t
  val compare: t -> t -> int
  val const: t
  val dst: var * var -> t
  val map: (var -> var) -> t -> t option
  val local: VSet.t -> t -> bool
  val obj: t -> int
  val fold: ('a -> t -> 'a) -> 'a -> VSet.t -> 'a
  val range: pstate -> t -> (int option * int option)
  val printk: float -> t -> unit
end = struct
  type t = Const | Dst of var * var
  let compare = compare
  let const = Const
  let dst (u, v) = assert (u <> v); Dst (u, v)
  let map f = function
    | Const -> Some Const
    | Dst (a, b) ->
      let a' = f a and b' = f b in
      if a' = b' then None else
      Some (Dst (a', b'))
  let local vs = function
    | Const -> false
    | Dst (VNum _, a) | Dst (a, VNum _) -> VSet.mem a vs
    | Dst (a, b) -> VSet.mem a vs && VSet.mem b vs
  let obj = function
    | Dst (VNum a, VNum b) -> max (b-a) 0 + 10
    | Dst (VNum n, _)
    | Dst (_, VNum n) -> 10_000 + abs n
    | Dst _ -> 11_000
    | _ -> 1
  let fold f a vs =
    let vl = VSet.elements vs in
    let rec pairs a = function
      | v :: tl ->
        let g a v' = if v = v' then a else f a (dst (v, v')) in
        pairs (List.fold_left g a vl) tl
      | [] -> a
    in pairs (f a const) vl
  let range l = function
    | Dst (x1, x2) -> Logic.irange l x1 x2
    | _ -> (None, None)
  let printk k i =
    if abs_float k < 1e-6 then () else
    match i with
    | Const ->
      Printf.printf "%.2f\n" k
    | Dst (v1, v2) ->
      Printf.printf "%.2f |[%a, %a]|\n" k
        Parse.pp_var v1 Parse.pp_var v2
end

(* quantitative contexts and their operations *)
module Q: sig
  type ctx
  val vars: ctx -> VSet.t
  val empty: ctx
  val addv: ?sign: int -> ctx -> VSet.t -> ctx
  val delv: ?zero: bool -> ctx -> VSet.t -> ctx
  val inc: ctx -> (Idx.t * (Idx.t * Idx.t) list * int) list -> ctx
  val eqc: ctx -> ctx -> unit
  val relax: pstate -> ctx -> ctx
  val merge: ?sign: int -> ctx list -> ctx
  val free: ctx -> Idx.t -> ctx
  val lift: ?sign: int -> ctx -> ctx -> ctx
  val subst: ctx -> id list -> var list -> ctx
  val frame: ctx -> ctx -> ctx * ctx
  val restore: ctx -> ctx -> VSet.t -> ctx
  val solve: ctx * ctx -> unit
end = struct
  module M = Map.Make(Idx)
  type ctx = { cvars: VSet.t; cmap: int M.t }

  let newv ?(sign=0) () =
    Clp.add_column
      { Clp.column_obj = 0.
      ; Clp.column_lower = if sign <= 0 then -. max_float else 0.
      ; Clp.column_upper = if sign >= 0 then max_float else 0.
      ; Clp.column_elements = [| |]
      };
    if debug > 3 && sign <> 0 then
      Printf.printf "sign of %d is %d\n" (Clp.number_columns () - 1) sign;
    Clp.number_columns () - 1

  let row ?(lo=0.) ?(up=0.) v' l k =
    let row_elements = Array.of_list begin
      (v', 1.) :: List.map
        (fun (i, w) -> (i, float_of_int (-w))) l
    end in
    if debug > 1 then begin
      if lo <> 0. || up <> 0. then () else
      let open Printf in
      let c = function
        | (v, w) when w = 1 -> sprintf "v%d" v
        | (v, w) -> sprintf "%d * v%d" w v in
      printf "v%d = %d" v' k;
      List.iter (fun x -> printf " + %s" (c x)) l;
      print_newline ()
    end;
    Clp.add_row
      { Clp.row_lower = float_of_int k +. lo
      ; Clp.row_upper = float_of_int k +. up
      ; row_elements
      }

  let gerow v1 v2 c =
    row ~lo:0. ~up:max_float v1 [(v2, c)] 0

  let vars c = c.cvars

  let empty = {cvars = VSet.empty; cmap = M.empty}

  let addv ?(sign=0) c vs =
    assert (VSet.is_empty (VSet.inter (vars c) vs));
    let cvars = VSet.union [vars c; vs] in
    let cmap = Idx.fold
      (fun m i ->
        let v =
          try M.find i c.cmap with
          | Not_found -> newv ~sign () in
        M.add i v m)
      M.empty cvars in
    {cvars; cmap}

  let inc ({cmap=m;_} as c) eqs =
    let bdgs = List.map
      begin fun (id, l, k) ->
        let mkmax (i1, i2) =
          let v = newv () in
          if debug > 1 then
            Printf.printf "v%d >= max(v%d, -v%d)\n" v
              (M.find i1 m) (M.find i2 m);
          gerow v (M.find i1 m) (+1);
          gerow v (M.find i2 m) (-1);
          (v, 1) in
        let v = newv () in
        row v ((M.find id m, 1) :: List.map mkmax l) k;
        (id, v)
      end eqs in
    { c with cmap =
      List.fold_left (fun m (i, v) -> M.add i v m) m bdgs }

  let delv ?(zero=true) c vs =
    assert (VSet.subset vs (vars c));
    let cvars = VSet.diff (vars c) vs in
    let m _ vo vo' =
      match vo, vo' with
      | Some _, Some _ -> vo
      | Some v, None -> if zero then row v [] 0; None
      | _, _ -> assert false in
    let c' = Idx.fold (fun m i -> M.add i 0 m) M.empty cvars in
    {cvars; cmap = M.merge m c.cmap c'}

  let eqc c1 c2 =
    assert (VSet.equal c1.cvars c2.cvars);
    let eqv i = row (M.find i c1.cmap) [(M.find i c2.cmap, 1)] 0 in
    Idx.fold (fun () -> eqv) () c1.cvars

  let relax ps c =
    let lo, eq, up =
      Idx.fold begin fun (lo, eq, up) i ->
        match Idx.range ps i with
        | Some k1, Some k2 ->
          if k1 >= k2
          then (lo, (i, k1) :: eq, up)
          else ((i, k1) :: lo, eq, (i, k2) :: up)
        | Some k, _ -> ((i, k) :: lo, eq, up)
        | _, Some k -> (lo, eq, (i, k) :: up)
        | _ -> (lo, eq, up)
      end ([], [], []) c.cvars in
    let l =
      List.map (fun (i, k) -> (i, (newv ~sign:(-1) (), k))) lo @
      List.map (fun (i, k) -> (i, (newv ~sign:0 (), k))) eq @
      List.map (fun (i, k) -> (i, (newv ~sign:(+1) (), k))) up in
    if l = [] then c else
    let c = List.fold_left
      begin fun c (i, (ip, _)) ->
        let v' = newv () in
        row v' [(M.find i c.cmap, 1); (ip, -1)] 0;
        { c with cmap = M.add i v' c.cmap }
      end c l in
    let v' = newv () and ic = Idx.const in
    row v' ((M.find ic c.cmap, 1) :: List.map snd l) 0;
    { c with cmap = M.add ic v' c.cmap }

  let merge ?(sign=0) cl =
    assert (List.for_all
      (fun {cvars;_} -> VSet.equal cvars (List.hd cl).cvars) cl
    );
    let m cmap' i =
      let v' = newv ~sign () in
      if debug > 1 then begin
        Printf.printf "v%d >= max(" v';
        ignore (List.fold_left (fun c {cmap=m;_} ->
            Printf.printf "%sv%d" (if c then ", " else "") (M.find i m);
            true
          ) false cl);
        Printf.printf ")\n";
      end;
      List.iter (fun {cmap;_} -> gerow v' (M.find i cmap) 1) cl;
      M.add i v' cmap' in
    let cvars = (List.hd cl).cvars in
    {cvars; cmap = Idx.fold m M.empty cvars}

  let free c idx =
    if debug > 1 then Printf.printf "v%d <= 0\n" (M.find idx c.cmap);
    row ~lo:(-. max_float) ~up:0. (M.find idx c.cmap) [] 0;
    { c with cmap = M.add idx (newv ~sign:(+1) ()) c.cmap }

  let lift ?(sign=(+1)) q q' =
    (* assert (VSet.subset (vars q) (vars q')); *)
    addv ~sign q (VSet.diff (vars q') (vars q))

  let subst {cvars; cmap} xl vl =
    let rename =
      let h = Hashtbl.create 11 in
      List.iter2 (Hashtbl.add h) xl vl;
      function
      | VId id as v ->
        (try Hashtbl.find h id with Not_found -> v)
      | VNum _ as v -> v in
    let h = Hashtbl.create 257 in
    Idx.fold (fun () i ->
      match Idx.map rename i with
      | Some i' -> Hashtbl.add h i' i
      | None -> ()
    ) () cvars;
    let cmap = Idx.fold (fun m i ->
        match Hashtbl.find_all h i with
        | [] -> M.add i (newv ~sign:(+1) ()) m
        | [i'] -> M.add i (M.find i' cmap) m
        | payfor ->
          let v = newv () in
          row v (List.map (fun i -> (M.find i cmap,1)) payfor) 0;
          M.add i v m
      ) M.empty cvars in
    {cvars; cmap}

  let frame c1 c2 =
    let vx = newv ~sign:(+1) () and v1 = newv () and v2 = newv () in
    row v1 [(M.find Idx.const c1.cmap, 1); (vx, 1)] 0;
    row v2 [(M.find Idx.const c2.cmap, 1); (vx, 1)] 0;
    ( {c1 with cmap = M.add Idx.const v1 c1.cmap}
    , {c2 with cmap = M.add Idx.const v2 c2.cmap }
    )

  let restore c c' locals =
    assert (VSet.subset locals (vars c'));
    assert (VSet.subset locals (vars c));
    Idx.fold (fun c i ->
        if not (Idx.local locals i) then c else
        {c with cmap = M.add i (M.find i c'.cmap) c.cmap}
      ) c (vars c)

  let solve (cini, cfin) =
    let obj = Clp.objective_coefficients () in
    Idx.fold begin fun () i ->
      let o = float_of_int (Idx.obj i) in
      let v = M.find i cini.cmap in
      row ~lo:0. ~up:max_float v [] 0;
      obj.(v) <- o
    end () cini.cvars;
    Clp.change_objective_coefficients obj;
    flush stdout;
    Clp.set_log_level (if debug > 1 then 2 else 0);
    Clp.initial_solve ();
    match Clp.status () with
    | 0 ->
      let sol = Clp.primal_column_solution () in
      if debug > 1 then begin
        print_string "\nCoefficients:\n";
        for i = 0 to Array.length sol - 1 do
           if abs_float sol.(i) > 1e-6 then
           Printf.printf "v%d = %.2f\n" i sol.(i)
        done;
        print_newline ()
      end;
      let p c = Idx.fold
        (fun () i -> Idx.printk sol.(M.find i c.cmap) i)
        () (vars c) in
      p cini;
      if debug > 0 then (print_string "Final annotation:\n"; p cfin)
    | _ -> print_string "Sorry, I could not find a bound.\n"

end


let analyze (fdefs, p) =
  (* generate and resolve constraints *)
  let open Idx in

  let glos = VSet.add (VNum 0) (file_globals (fdefs, p)) in
  let tmpret = "%ret" and vret = VId "%ret" in
  let rec gen_ qfuncs qret qbrk qseq =
    let gen = gen_ qfuncs qret qbrk in function

    | PTick (n, _) ->
      if n = 0 then qseq else
      let q = Q.inc qseq [const, [], n] in
      if n < 0 then Q.merge ~sign:(+1) [q] else q
    | PAssert _ -> qseq
    | PBreak {lpre; _} -> Q.relax lpre qbrk
    | PReturn (v, _) ->
      let q = Q.lift qret qseq in
      Q.delv (Q.subst q [tmpret] [v]) ~zero:false (VSet.of_list [vret])

    | PCall (ret, fname, args, _) ->
      let varl = List.map (fun x -> VId x) in
      let f = List.find (fun x -> x.fname = fname) fdefs in

      let tmps = List.map ((^) "%") f.fargs in
      let tmpset = VSet.of_list (varl tmps) in
      let argset = VSet.of_list (varl f.fargs) in
      let locset = VSet.of_list (varl f.flocs) in

      let qret = Q.addv ~sign:(+1) qseq (VSet.of_list [vret]) in
      let qret =
        match ret with
        | Some x -> Q.subst qret [x] [vret] (* XXX move *)
        | None -> qret in

      begin match
        try Some (List.assoc fname qfuncs)
        with Not_found -> None
      with

      | None -> (* unfold case *)
        let qcall = Q.addv ~sign:(+1) Q.empty (VSet.union [tmpset; Q.vars qseq]) in
        let qfun = Q.delv qret ~zero:false (VSet.of_list [vret]) in
        let qfun = Q.addv ~sign:(+1) qfun (VSet.union [argset; locset]) in
        let qbdy = gen_
          ((fname, (qcall, qret)) :: qfuncs)
          qret Q.empty qfun f.fbody in
        let qcall' = Q.addv ~sign:(+1) qbdy tmpset in
        let qcall' = Q.subst qcall' f.fargs (varl tmps) in (* XXX move *)
        let qcall' = Q.delv qcall' (VSet.union [argset; locset]) in
        Q.eqc qcall qcall';
        let q = Q.subst qcall tmps args in
        Q.delv q ~zero:false tmpset

      | Some (qcallf, qretf) -> (* recursive case *)
        let qcallf, qretf = Q.frame qcallf qretf in
        let qcallf = Q.lift qcallf qseq in
        let qcall = Q.subst qcallf tmps args in
        let qcall = Q.delv ~zero:false qcall tmpset in
        let qretf = Q.lift ~sign:(-1) qretf qseq in
        let locs = VSet.diff (Q.vars qseq) glos in
        let qretf = Q.restore qretf qcallf locs in
        Q.eqc qret qretf; qcall

      end

    | PInc (x, op, y, {lpre; lpost}) ->
      let vars = VSet.remove (VId x) (Q.vars qseq) in
      let eqs =
        let opy, iopyz, izopy =
          let iyz = dst (y, VNum 0) and izy = dst (VNum 0, y) in
          match op with
          | OPlus -> LVar y, iyz, izy
          | OMinus -> LMult (-1, LVar y), izy, iyz in
        let sum opy = VSet.fold
          begin fun v sum ->
            if opy > 0
            then (dst (v, VId x), dst (VId x, v)) :: sum
            else (dst (VId x, v), dst (v, VId x)) :: sum
          end vars [] in
        match
          Logic.entails lpre opy CLe (LVar (VNum 0)),
          Logic.entails lpre opy CGe (LVar (VNum 0))
        with
        | true, true -> []
        | false, false ->
          [iopyz, sum (-1), 0; izopy, sum (+1), 0]
        | true, false -> (* op y < 0 *)
          [iopyz, sum (-1), 0]
        | false, true -> (* op y > 0 *)
          [izopy, sum (+1), 0]
      in
      let q = Q.relax lpost qseq in
      Q.inc q eqs (* transfer potential to +y or -y *)

    | PSet (x, Some v, {lpre; _}) ->
      let q = Q.subst qseq [x] [v] in
      (* relax constant differences *)
      Q.relax lpre q

    | PSet (x, None, _) ->
      let vars = VSet.remove (VId x) (Q.vars qseq) in
      VSet.fold begin fun u q ->
        Q.free (Q.free q (dst (u, VId x))) (dst (VId x, u))
      end vars qseq

    | PSeq (p1, p2, _) ->
      let qpre2 = gen qseq p2 in
      let qpre1 = gen (Q.merge [qpre2]) p1 in
      qpre1

    | PLoop (p, {lpre; _}) ->
      let qinv = Q.merge [qseq] in
      let qinv' = gen_ qfuncs qret qseq qinv p in
      Q.eqc qinv qinv';
      qinv'

    | PIf (_, p1, p2, _) ->
      let qpre1 = gen qseq p1 in
      let qpre2 = gen qseq p2 in
      Q.merge [qpre1; qpre2]

    in

  let q = Q.addv ~sign:(+1) Q.empty glos in
  let qret = Q.addv ~sign:(+1) q (VSet.singleton (VId "%ret")) in
  let qpre = gen_ [] qret Q.empty q p in
  let qpre = Q.relax (prog_data p).lpre qpre in
  Q.solve (qpre, q)


let _ =
  let f () = Tools.clean_file (Parse.pa_file stdin) in
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "-tq" then
    let f = Tools.auto_tick (f ()) in
    let f = lannot f in
    analyze f
  else if Array.length Sys.argv > 1 && Sys.argv.(1) = "-tlannot" then
    let f = lannot (f ()) in
    let pre {lpre; lpost} =
      Logic.pp lpre; print_string "\n"
    and post {lpre; lpost} =
      print_string "\n"; Logic.pp lpost
    in Parse.pp_file_hooks pre post f
