
let print_proof_term () =
  let p = try Proof_global.give_me_the_proof () with
        Not_found -> failwith "give_me_the_proof:notfound" in
  let pprf = try Proof.partial_proof p with
               Not_found -> failwith "partial_proof:not found" in
  List.iter PpCoq.print_constr (List.map EConstr.Unsafe.to_constr pprf)

let print_reference r =
  Format.printf "%a@."
    PpCoq.pp_global_reference
    (Constrintern.intern_reference r)
  
let print_constr cin =
  let env = Global.env () in
  let evd = Evd.from_env env in
  let c, evd = Constrintern.interp_type env evd cin in
  PpCoq.print_constr (EConstr.Unsafe.to_constr c)
