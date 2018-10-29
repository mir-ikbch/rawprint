(* $Id: print.ml,v 1.6 2013/08/19 02:01:07 ksk Exp $ *)
(* Common functions for pretty printers (PP) *)
(* Copyright (C) 2005-2013 Keisuke Nakano. All rights reserved.*)

open Format

let strbuf = Buffer.create 1024

let string_of pp_a a =
  let prf = formatter_of_buffer strbuf in
  pp_a prf a; pp_print_flush prf ();
  let str = Buffer.contents strbuf in
  Buffer.clear strbuf;
  str

(* PP for built-in types *)
let pp_unit prf () = pp_print_string prf "()"
let pp_bool = pp_print_bool
let pp_int = pp_print_int
let pp_float = pp_print_float
let pp_char prf = fprintf prf "%C"
let pp_string prf = fprintf prf "%S"
let pp_format6 _ prf fmt = pp_string prf (string_of_format fmt)

(* PP for lists with a specified separation *)
let pp_list_with sep pp_a prf = function
  | [] -> ()
  | x::xs ->
      pp_a prf x;
      List.iter (fprintf prf "%s@;%a" sep pp_a) xs

(* PP for lists *)
let pp_list pp_a prf xs =
  fprintf prf "@[<1>[%a]@]" (pp_list_with ";" pp_a) xs

(* PP for arrays *)
let pp_array pp_a prf xs =
  fprintf prf "@[<2>[|";
  let len = Array.length xs in
  if len > 0 then begin
    pp_a prf xs.(0);
    for i=1 to len-1 do fprintf prf ";@;%a" pp_a xs.(i) done end;
  fprintf prf "|]@]"

(* Heterogeneous list based on existing type variable emulation *)
type pp_poly = { pp_poly: 'b. 'b pp_neg -> 'b }
and 'b pp_neg = { pp_neg: 'a. (formatter -> 'a -> unit) -> 'a -> 'b }
let pp_poly pp_a x = { pp_poly = fun k -> k.pp_neg pp_a x }
let apply_pp_poly prf p = p.pp_poly { pp_neg = fun pp_a -> pp_a prf }

(* prefix operator for pp_poly *)
let (!%) pp_a = pp_poly pp_a

let rec string_forall p str i j =
  j < i || p str.[i] && string_forall p str (i+1) j 

(* PP for heterogeneous lists *)
let pp_poly_list prf = function
  | [] -> ()
  | [p] ->
    (* Fisrt check the first character of s.
       It is quite inefficient due to formatting twice ... *)
    let s = string_of apply_pp_poly p in 
    if s = "" then ()
    else
      let is_atom = match s.[0]  with
        | '(' | '[' | '{' | '<' | '"' | '\'' -> true
        | _ -> string_forall (function
            | '0'..'9' | 'A'..'Z' | '_' | 'a'..'z' -> true
            | _ -> false) s 1 (String.length s - 1) in
      if is_atom then fprintf prf " %a" apply_pp_poly p
      else fprintf prf "(%a)" apply_pp_poly p
  | p::ps ->
    fprintf prf "@[<1>(%a" apply_pp_poly p;
    List.iter (fprintf prf ",@;%a" apply_pp_poly) ps;
    fprintf prf ")@]"

(* PP for tuples *)
let pp_tuple (make_pps:'a -> pp_poly list) prf x = pp_poly_list prf (make_pps x)

(* PP for pairs *)
let pp_tuple_2 (pp_a, pp_b) prf x =
  pp_tuple (fun (a,b) -> [ !%pp_a a; !%pp_b b]) prf x

(* PP for triples *)
let pp_tuple_3 (pp_a, pp_b, pp_c) prf x =
  pp_tuple (fun (a,b,c) -> [ !%pp_a a; !%pp_b b; !%pp_c c ]) prf x

(* PP for quadruples *)
let pp_tuple_4 (pp_a, pp_b, pp_c, pp_d) prf x =
  pp_tuple (fun (a,b,c,d) -> [ !%pp_a a; !%pp_b b; !%pp_c c; !%pp_d d ]) prf x

(* PP for quintuples *)
let pp_tuple_5 (pp_a, pp_b, pp_c, pp_d, pp_e) prf x =
  pp_tuple (fun (a,b,c,d,e) -> [ !%pp_a a; !%pp_b b; !%pp_c c; !%pp_d d; !%pp_e e ]) prf x

(* PP for sextuples *)
let pp_tuple_6 (pp_a, pp_b, pp_c, pp_d, pp_e, pp_f) prf x =
  pp_tuple (fun (a,b,c,d,e,f) -> [ !%pp_a a; !%pp_b b; !%pp_c c;
                                   !%pp_d d; !%pp_e e; !%pp_f f ]) prf x
    
(* PP for septuples *)
let pp_tuple_7 (pp_a, pp_b, pp_c, pp_d, pp_e, pp_f, pp_g) prf x =
  pp_tuple (fun (a,b,c,d,e,f,g) -> [ !%pp_a a; !%pp_b b; !%pp_c c;
                                     !%pp_d d; !%pp_e e; !%pp_f f; !%pp_g g ]) prf x

(* PP for variants *)
let pp_variant (make_cps:'a -> string * pp_poly list) prf x =
  let constr_name, ps = make_cps x in
  fprintf prf "%s%a" constr_name pp_poly_list ps

(* PP for polymorphic variants sum, e.g., [`a of int | `b | known] *)
type pp_poly_variant_cps = PPConstr of string * pp_poly list | PPKnown of pp_poly
let pp_poly_variant (make_cps:'a -> pp_poly_variant_cps) prf x =
  match make_cps x with
    | PPConstr(c,ps) -> pp_variant (fun () -> c, ps) prf ()
    | PPKnown p -> apply_pp_poly prf p

(* PP for options *)
let pp_option pp_a =
  pp_variant (function
                | None -> "None", []
                | Some x -> "Some", [ !%pp_a x ])

(* PP for records and objects *)
let pp_recobj encL encR (make_pp_fields:'a -> (string * pp_poly) list) prf x =
  let apply_pp_field prf (f,p) = fprintf prf "@[<2>%s = @,%a@]" f apply_pp_poly p in
  fprintf prf "@[<1>%s" encL;
  begin match make_pp_fields x with
    | [] -> ()
    | fp::fps ->
        apply_pp_field prf fp;
        List.iter (fprintf prf ";@;%a" apply_pp_field) fps end;
  fprintf prf "%s@]" encR

let pp_record mpf = pp_recobj "{" "}" mpf
let pp_object mpf = pp_recobj "<" ">" mpf


(* module Pervasives *)
let pp_fpclass prf = pp_variant (function
  | FP_normal -> "FP_normal", []
  | FP_subnormal -> "FP_subnormal", []
  | FP_zero -> "FP_zero", []
  | FP_infinite -> "FP_infinite", []
  | FP_nan -> "FP_nan", []) prf
let pp_in_channel prf (_:in_channel) = pp_print_string prf "<in_channel>"
let pp_out_channel prf (_:out_channel) = pp_print_string prf "<out_channel>"

let pp_open_flag prf = pp_variant (function
  | Open_rdonly -> "Open_rdonly", []
  | Open_wronly -> "Open_wronly", []
  | Open_append -> "Open_append", []
  | Open_creat -> "Open_creat", []
  | Open_trunc -> "Open_trunc", []
  | Open_excl -> "Open_excl", []
  | Open_binary -> "Open_binary", []
  | Open_text -> "Open_text", []
  | Open_nonblock -> "Open_nonblock", []) prf

let pp_ref pp_a prf = pp_record (fun r -> ["contents", !%pp_a r.contents]) prf

let pp_format4 _ prf (fmt:(_,_,_,_) format4) = pp_string prf (string_of_format fmt)
let pp_format _ prf (fmt:(_,_,_) format) = pp_string prf (string_of_format fmt)

(* let pp_exn prf = pp_variant (function *)
(*   | Not_found -> "Not_found", []) prf *)
let pp_exn prf exn = pp_print_string prf (Printexc.to_string exn)

(* module String *)
module String = struct
  include String
  let pp_t = pp_string
end

(* module Hashtbl *)
module Hashtbl = struct
  (* include Hashtbl excluding HashedType, S, and Make *)
  type ('a,'b) t = ('a,'b) Hashtbl.t
  let create = Hashtbl.create
  let clear = Hashtbl.clear
  let add = Hashtbl.add
  let copy = Hashtbl.copy
  let find = Hashtbl.find
  let find_all = Hashtbl.find_all
  let mem = Hashtbl.mem
  let remove = Hashtbl.remove
  let replace = Hashtbl.replace
  let iter = Hashtbl.iter
  let fold = Hashtbl.fold
  let length = Hashtbl.length
  let hash = Hashtbl.hash
  let hash_param = Hashtbl.hash_param
  let gen_pp_t pp_a pp_b h_iter prf t =
    fprintf prf "@[<1>{";
    let pp_each key prf v = fprintf prf "@[<2>%a => @,%a@]" pp_a key pp_b v in
    let is_fst = ref true in
    h_iter (fun key v ->
      if !is_fst then begin pp_each key prf v; is_fst := false end
      else fprintf prf ";@;%a" (pp_each key) v) t;
    fprintf prf "}@]"
  let pp_t (pp_a,pp_b) = gen_pp_t pp_a pp_b iter
  module type HashedType = sig
    include Hashtbl.HashedType
    val pp_t : Format.formatter -> t -> unit
  end
  module type S = sig
    include Hashtbl.S
    val pp_t : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
  module Make(H : HashedType) : S with type key = H.t = struct
    include Hashtbl.Make(H)
    let pp_t pp_a = gen_pp_t H.pp_t pp_a iter
  end
end

(* module Set *)
module Set = struct
  module type OrderedType = sig
    include Set.OrderedType
    val pp_t : Format.formatter -> t -> unit
  end
  module type S = sig
    include Set.S
    val pp_elt : Format.formatter -> elt -> unit
    val pp_t : Format.formatter -> t -> unit
  end
  module Make(Ord:OrderedType) : S with type elt = Ord.t = struct
    module S = Set.Make(Ord)
    include S
    let pp_elt = Ord.pp_t
    let pp_t prf set =
      fprintf prf "@[<1>{";
      ignore (S.fold (fun elt is_fst ->
                        if is_fst then pp_elt prf elt
                        else fprintf prf ",@;%a" pp_elt elt;
                        false) set true);
      fprintf prf "}@]"
  end
end

(* module Map *)
module Map = struct
  module type OrderedType = sig
    include Map.OrderedType
    val pp_t : Format.formatter -> t -> unit
  end
  module type S = sig
    include Map.S
    val find_some : key -> 'a t -> 'a option
    val pp_key : Format.formatter -> key -> unit
    val pp_t : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
  module Make(Ord:OrderedType) : S with type key = Ord.t = struct
    module M = Map.Make(Ord)
    include M
    let pp_key = Ord.pp_t
    let find_some key t = try Some(M.find key t) with Not_found -> None
    let pp_t pp_a prf map =
      fprintf prf "@[<1>{";
      let pp_each key prf v = fprintf prf "@[<2>%a => @,%a@]" pp_key key pp_a v in
      ignore (M.fold (fun key v is_fst ->
                        if is_fst then pp_each key prf v
                        else fprintf prf ";@;%a" (pp_each key) v;
                        false) map true);
      fprintf prf "}@]"
  end
end
