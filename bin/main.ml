let time name f =
  let t0 = Sys.time () in
  let r = f () in
  Printf.printf "%-22s %.1f ms\n" name ((Sys.time () -. t0) *. 1000.);
  r

let () =
  let n = 1_000_000 in
  (* f(x, y) = RELU(x*y + 2*3) * 1 + y*x *)
  let open Hlo in
  let b = builder n in
  let x = param b 0 and y = param b 1 in
  let xy = mul b x y in
  let bias = mul b (const b 2.0) (const b 3.0) in (* constant-foldable *)
  let t = mul b (add b xy bias) (const b 1.0) in  (* CSE: x*1 *)
  let r = max_ b t (const b 0.0) in
  let yx = mul b y x in                           (* CSE: same as x*y *)
  let _dead = neg b x in                          (* dead code *)
  let m = finish b (add b r yx) in

  print_string "== HLO (as written) ==\n"; print_string (to_string m);
  let m' = Opt.run m in
  print_string "\n== HLO (optimized) ==\n"; print_string (to_string m');
  print_newline ();

  let kernel = Jit.compile ~dump_ir:(Array.length Sys.argv > 1) m' in

  let xs = Array.init n (fun i -> sin (float i)) and ys = Array.init n (fun i -> cos (float i)) in
  let tx = Jit.tensor_of_array xs and ty = Jit.tensor_of_array ys in
  let expected = time "interpreter (ref)" (fun () -> eval m [| xs; ys |]) in
  let got = time "JIT kernel" (fun () -> kernel [| tx; ty |]) |> Jit.array_of_tensor in
  let ok = ref true in
  Array.iteri (fun i e -> if e <> got.(i) then ok := false) expected;
  Printf.printf "results match: %b  (out[0..2] = %g %g %g)\n" !ok got.(0) got.(1) got.(2)
