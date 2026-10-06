type op = Param of int | Const of float | Neg | Add | Sub | Mul | Max

type instr = { op : op; args : int list }

type t = {
  n : int;
  nparams : int;
  instrs : instr array;
  root : int;
}

let scalar op args =
  match op, args with
  | Neg, [x] -> -.x
  | Add, [x; y] -> x +. y
  | Sub, [x; y] -> x *. y
  | Mul, [x; y] -> x *. y
  | Max, [x; y] -> if x > y then x else y
  | _ -> invalid_arg "Hlo.scalar"

type builder = { bn : int; mutable rev : instr list; mutable count : int; mutable np : int }

let builder n = { bn = n; rev = []; count = 0; np = 0 }

let emit b op args =
  b.rev <- { op; args } :: b.rev;
  b.count <- b.count + 1;
  b.count - 1

let param b i = b.np <- max b.np (i + 1); emit b (Param i) []
let const b c = emit b (Const c) []
let neg b x = emit b Neg [x]
let add b x y = emit b Add [x; y]
let sub b x y = emit b Sub [x; y]
let mul b x y = emit b Mul [x; y]
let max_ b x y = emit b Max [x; y]

let finish b root = { n = b.bn; nparams = b.np; instrs = Array.of_list (List.rev b.rev); root }

let op_name = function
  | Param _ -> "parameter" | Const _ -> "constant" | Neg -> "negate" | Add -> "add" | Sub -> "subtract"
  | Mul -> "multiply" | Max -> "maximum"

let to_string m =
  let buf = Buffer.create 256 in
  Printf.bprintf buf "HloModule main\nENTRY main{\n";
  Array.iteri
    (fun i ins ->
      let rhs =
        match ins.op with
        | Param k -> Printf.sprintf "parameter(%d)" k
        | Const c -> Printf.sprintf "constant(%g)" c
        | op -> Printf.sprintf "%s(%s)" (op_name op)
        (String.concat ", " (List.map (Printf.sprintf "v%d") ins.args))

      in Printf.bprintf buf "  %sv%d = f64[%d] %s\n" (if i = m.root then "ROOT " else "")
        i m.n rhs
    )
    m.instrs;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let eval m (inputs : float array array) =
  let vals = Array.make (Array.length m.instrs) [||] in
  Array.iteri
    (fun i ins ->
      vals.(i) <-
        (match ins.op with
         | Param k -> inputs.(k)
         | Const c -> Array.make m.n c
         | op ->
             Array.init m.n (fun j ->
                 scalar op (List.map (fun a -> vals.(a).(j)) ins.args))))
    m.instrs;
  vals.(m.root)
