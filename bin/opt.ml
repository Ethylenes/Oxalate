open Hlo

let run (m : Hlo.t) : Hlo.t =
  let defs : (int, instr) Hashtbl.t = Hashtbl.create 16 in
  let cse : (instr, int) Hashtbl.t = Hashtbl.create 16 in
  let next = ref 0 in
  let emit ins =
    match Hashtbl.find_opt cse ins with
    | Some id -> id
    | None ->
      let id = !next in
      incr next;
      Hashtbl.add defs id ins;
      Hashtbl.add cse ins id;
      id
  in
  let const_of id = match (Hashtbl.find defs id).op with Const c -> Some c | _ -> None in
  let simplify op args =
  let cs = List.map const_of args in
    match op, args, cs with
    | (Param _ | Const _), _, _ -> emit { op; args }
    | _, _, _ when List.for_all Option.is_some cs -> (* constant folding *)
      emit { op = Const (scalar op (List.map Option.get cs)); args = [] }
    | Add, [x; _], [_; Some 0.] -> x (* Zero Identity Addition *)
    | Add, [_; y], [Some 0.; _] -> y
    | Sub, [x; _], [_; Some 0.] -> x (* Zero Identity Subtraction *)
    | Mul, [x; _], [_; Some 1.] -> x (* Multiplication Identity Property *)
    | Mul, [_; y], [Some 1.; _] -> y
    | (Add | Mul), [x; y], _ when x > y -> emit { op; args = [y; x] } (* canonical order => better CSE *)
    | _ -> emit { op; args }
  in
  let remap = Array.make (Array.length m.instrs) (-1) in
  Array.iteri
    (fun i ins -> remap.(i) <- simplify ins.op (List.map (fun a -> remap.(a)) ins.args))
    m.instrs;
  let root = remap.(m.root) in
  let live = Array.make !next false in
  let rec mark id =
    if not live.(id) then (live.(id) <- true; List.iter mark (Hashtbl.find defs id).args)
  in
  mark root;
  let renum = Array.make !next (-1) and k = ref 0 and out = ref [] in
  for id = 0 to !next - 1 do
    if live.(id) then begin
      let ins = Hashtbl.find defs id in
      renum.(id) <- !k;
      incr k;
      out := { ins with args = List.map(fun a -> renum.(a)) ins.args } :: !out
    end
  done;
  { m with instrs = Array.of_list (List.rev !out); root = renum.(root) }
