open Hlo

type tensor = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

let tensor_of_array (a : float array) : tensor = Bigarray.Array1.of_array Bigarray.Float64 Bigarray.C_layout a
let array_of_tensor (t : tensor) = Array.init (Bigarray.Array1.dim t) (Bigarray.Array1.get t)


(* Lower to: void kernel(double** params, double* out) *)
let lower (m : Hlo.t) : Llvm.llmodule =
  assert (m.n > 0);
  let ctx = Llvm.global_context () in
  let md = Llvm.create_module ctx "microxla" in
  let b = Llvm.builder ctx in
  let f64 = Llvm.double_type ctx and i64 = Llvm.i64_type ctx in
  let ptr = Llvm.pointer_type ctx in
  let fn = Llvm.define_function "kernel"
    (Llvm.function_type (Llvm.void_type ctx) [| ptr; ptr |]) md in
  let params = Llvm.param fn 0 and out = Llvm.param fn 1 in
  let entry = Llvm.entry_block fn in
  let loop = Llvm.append_block ctx "loop" fn in
  let exit_ = Llvm.append_block ctx "exit" fn in
  (* entry: load each input's base pointer *)
  Llvm.position_at_end entry b;
  let bases = Array.init m.nparams (fun k ->
    let slot = Llvm.build_gep ptr params [| Llvm.const_int i64 k |] "" b in
    Llvm.build_load ptr slot (Printf.sprintf "p%d" k) b) in
  ignore (Llvm.build_br loop b);

  Llvm.position_at_end loop b;
  let i = Llvm.build_phi [ (Llvm.const_int i64 0, entry) ] "i" b in
  let v = Array.make (Array.length m.instrs) i in
  Array.iteri (fun id ins ->
    let a k = v.(List.nth ins.args k) in
    v.(id) <-
      (match ins.op with
      | Param k ->
        let p = Llvm.build_gep f64 bases.(k) [| i |] "" b in
        Llvm.build_load f64 p "" b
        | Const c -> Llvm.const_float f64 c
        | Neg -> Llvm.build_fneg (a 0) "" b
        | Add -> Llvm.build_fadd (a 0) (a 1) "" b
        | Sub -> Llvm.build_fsub (a 0) (a 1) "" b
        | Mul -> Llvm.build_fmul (a 0) (a 1) "" b
        | Max ->
          let gt = Llvm.build_fcmp Llvm.Fcmp.Ogt (a 0) (a 1) "" b in
          Llvm.build_select gt (a 0) (a 1) "" b))
    m.instrs;
  let dst = Llvm.build_gep f64 out [| i |] "" b in
  ignore (Llvm.build_store v.(m.root) dst b);
  let next = Llvm.build_add i (Llvm.const_int i64 1) "i.next" b in
  Llvm.add_incoming (next, loop) i;
  let fin = Llvm.build_icmp Llvm.Icmp.Eq next (Llvm.const_int i64 m.n) "" b in
  ignore (Llvm.build_cond_br fin exit_ loop b);
  Llvm.position_at_end exit_ b;
  ignore (Llvm.build_ret_void b);
  (match Llvm_analysis.verify_module md with
    | Some err -> failwith ("invalid LLVM module: " ^ err)
    | None -> ());
  md

(* Run -O3 optimization  *)
let optimize (md : Llvm.llmodule) =
  Llvm_all_backends.initialize ();
  let triple = Llvm_target.Target.default_triple () in
  let tm = Llvm_target.TargetMachine.create ~triple (Llvm_target.Target.by_triple triple) in
  Llvm.set_target_triple triple md;
  Llvm.set_data_layout (Llvm_target.DataLayout.as_string (Llvm_target.TargetMachine.data_layout tm)) md;
  let opts = Llvm_passbuilder.create_passbuilder_options () in
  (match Llvm_passbuilder.run_passes md "default<O3>" tm opts with
    | Ok () -> ()
    | Error e -> failwith e);
  Llvm_passbuilder.dispose_passbuilder_options opts

(* JIT-compile and return an OCaml closure *)
let compile ?(dump_ir = false) (m : Hlo.t) : tensor array -> tensor =
  ignore (Llvm_executionengine.initialize ());
  let md = lower m in
  if dump_ir then (print_endline "; ==== LLVM IR (before -O3) ===="; Llvm.dump_module md);
  optimize md;
  if dump_ir then (print_endline "; ==== LLVM IR (after -O3) ===="; Llvm.dump_module md);
  let ee = Llvm_executionengine.create md in
  let open Ctypes in
  let kernel =
    Llvm_executionengine.get_function_address "kernel"
      (Foreign.funptr (ptr (ptr double) @-> ptr double @-> returning void)) ee in
  fun inputs ->
    assert (Array.length inputs = m.nparams);
    let out : tensor = Bigarray.Array1.create Bigarray.Float64 Bigarray.C_layout m.n in
    let slots = CArray.make (ptr double) (max 1 m.nparams) in
    Array.iteri (fun k t -> CArray.set slots k (bigarray_start array1 t)) inputs;
    kernel (CArray.start slots) (bigarray_start array1 out);
    out
