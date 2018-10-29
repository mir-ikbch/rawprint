
open Format
open Names
open Print
open Term

let pp_id_t prf i = pp_string prf (Id.to_string i)

(* dummy printer for "Not Yet Implemented" *)
let (??) msg prf _ = fprintf prf "{{??:%s}}" msg 
(* Impremented but maybe too large *)
let (!!) msg prf _ = fprintf prf "{{!!:%s}}" msg 

(* use ppcmds printer *)
let pp_of pr prf x = fprintf prf "|@[pp$%s|@]" (Pp.string_of_ppcmds (pr x))
let pr_of pp_a x = Pp.str (string_of pp_a x)

let pp_name_t = pp_variant (function 
  | Name i -> "Name", [!%pp_id_t i]
  | Anonymous -> "Anonymous", []
)

let pp_evar_t prf evt = pp_int prf (Evar.repr evt) 
let pp_existential_key = pp_evar_t
let pp_pexistential pp_a = pp_tuple_2 (pp_existential_key, pp_array pp_a)

let pp_contents = pp_variant (function 
| Pos -> "Pos", [] 
| Null -> "Null", []
)
let pp_universe = pp_of Univ.Universe.pr
let pp_sorts_t = pp_variant (function 
| Prop c -> "Prop", [!%pp_contents c]
| Type u -> "Type", [!%pp_universe u]
)

let pp_cast_kind = pp_variant (function
| VMcast -> "VMcast", []
| NATIVEcast -> "NATIVEcast", []
| DEFAULTcast -> "DEFAULTcast", []
| REVERTcast -> "REVERTcast", []
)

let pp_instance_t = pp_of (Univ.Instance.pr Univ.Level.pr)
let pp_puniverses pp_a = pp_tuple_2 (pp_a, pp_instance_t)

let pp_constant prf c = pp_string prf (debug_string_of_con c)

let pp_mut_ind_t prf mi = pp_string prf (MutInd.to_string mi)

let pp_inductive = pp_tuple_2 (pp_mut_ind_t, pp_int)
let pp_constructor = pp_tuple_2 (pp_inductive, pp_int)

let pp_case_style = pp_variant (function 
| LetStyle -> "LetStyle", []
| IfStyle -> "IfStyle", []
| LetPatternStyle -> "LetPatternStyle", []
| MatchStyle -> "MatchStyle", []
| RegularStyle -> "RegularStyle", []
)
let pp_case_printing = pp_record (fun cp -> [
  "ind_tags",  !%(pp_list pp_bool) cp.ind_tags;
  "cstr_tags", !%(pp_array (pp_list pp_bool)) cp.cstr_tags;
  "style",     !%pp_case_style cp.style;
])                                 
let pp_case_info = pp_record (fun ci -> [
  "ci_ind",         !%pp_inductive ci.ci_ind;
  "ci_npar",        !%pp_int ci.ci_npar;
  "ci_cstr_ndecls", !%(pp_array pp_int) ci.ci_cstr_ndecls;
  "ci_cstr_nargs",  !%(pp_array pp_int) ci.ci_cstr_nargs;
  "ci_pp_info",     !%pp_case_printing ci.ci_pp_info;
])

let pp_prec_declaration (pp_constr, pp_types) =
  pp_tuple_3 (pp_array pp_name_t, pp_array pp_types, pp_array pp_constr)
let pp_pfixpoint (pp_constr, pp_types) =
  pp_tuple_2 (pp_tuple_2 (pp_array pp_int, pp_int),
              pp_prec_declaration (pp_constr, pp_types))

let pp_pcofixpoint (pp_constr, pp_types) =
  pp_tuple_2 (pp_int, pp_prec_declaration (pp_constr, pp_types))

let pp_projection prf p =
  pp_string prf (Names.Projection.to_string p)

let rec pp_constr prf = pp_variant (fun c -> match Constr.kind c with
  | Rel n -> "Rel", [!%pp_int n]
  | Var i -> "Var", [!%pp_id_t i]
  | Meta n -> "Meta", [!%pp_int n]
  | Evar cp -> "Evar", [!%(pp_pexistential pp_constr) cp]
  | Sort s -> "Sort", [!%pp_sorts_t s]
  | Cast(c,k,t) -> "Cast", [!%pp_constr c; !%pp_cast_kind k; !%pp_constr t]
  | Prod(n,t,c) -> "Prod", [!%pp_name_t n; !%pp_constr t; !%pp_constr c]
  | Lambda(n,t,c) -> "Lambda", [!%pp_name_t n; !%pp_constr t; !%pp_constr c]
  | LetIn(n,b,t,c) ->
     "LetIn", [!%pp_name_t n; !%pp_constr b; !%pp_constr t; !%pp_constr c]
  | App(c,l) -> "App", [!%pp_constr c; !%(pp_array pp_constr) l]
  | Const pc -> "Const", [!%(pp_puniverses pp_constant) pc]
  | Ind pi -> "Ind", [!%(pp_puniverses pp_inductive) pi]
  | Construct pc -> "Construct", [!%(pp_puniverses pp_constructor) pc]
  | Case(ci,p,c,bl) ->
     "Case", [!%pp_case_info ci; !%pp_constr p; !%pp_constr c;
              !%(pp_array pp_constr) bl]
  | Fix pf -> "Fix", [!%(pp_pfixpoint (pp_constr, pp_constr)) pf]
  | CoFix pc -> "CoFix", [!%(pp_pcofixpoint (pp_constr, pp_constr)) pc]
  | Proj(p,c) -> "Proj", [!%pp_projection p; !%pp_constr c]
) prf

open Retroknowledge
let pp_int31_field = pp_variant (function
| Int31Bits -> "Int31Bits", []
| Int31Type -> "Int31Type", []
| Int31Constructor -> "Int31Constructor", []
| Int31Twice -> "Int31Twice", []
| Int31TwicePlusOne -> "Int31TwicePlusOne", []
| Int31Phi -> "Int31Phi", []
| Int31PhiInv -> "Int31PhiInv", []
| Int31Plus -> "Int31Plus", []
| Int31PlusC -> "Int31PlusC", []
| Int31PlusCarryC -> "Int31PlusCarryC", []
| Int31Minus -> "Int31Minus", []
| Int31MinusC -> "Int31MinusC", []
| Int31MinusCarryC -> "Int31MinusCarryC", []
| Int31Times -> "Int31Times", []
| Int31TimesC -> "Int31TimesC", []
| Int31Div21 -> "Int31Div21", []
| Int31Div -> "Int31Div", []
| Int31Diveucl -> "Int31Diveucl", []
| Int31AddMulDiv -> "Int31AddMulDiv", []
| Int31Compare -> "Int31Compare", []
| Int31Head0 -> "Int31Head0", []
| Int31Tail0 -> "Int31Tail0", []
| Int31Lor -> "Int31Lor", []
| Int31Land -> "Int31Land", []
| Int31Lxor -> "Int31Lxor", []
)
let pp_field = pp_variant (function
| KInt31(s,f) -> "KInt31", [!%pp_string s; !%pp_int31_field f]
)
let pp_entry = pp_constr
let pp_retroknowledge_action = pp_variant (function
| RKRegister(f,e) -> "RKRegister", [!%pp_field f; !%pp_entry e]
)

open Declarations
open Context
let pp_ephemeron_key pp_a = ??"ephemeron_key" (* internally ref ref int *)
let pp_named_declaration = pp_variant (function
| Named.Declaration.LocalAssum(i,c) -> "LocalAssum", [!%pp_id_t i; !%pp_constr c]
| Named.Declaration.LocalDef(i,c,t) -> "LocalDef", [!%pp_id_t i; !%pp_constr c; !%pp_constr t]
)
let pp_named_context = pp_list pp_named_declaration
let pp_section_context = pp_named_context
let pp_rel_declaration = pp_variant (function
| Rel.Declaration.LocalAssum(na,c) -> "LocalAssum", [!%pp_name_t na; !%pp_constr c]
| Rel.Declaration.LocalDef(na,c,t) -> "LocalDef", [!%pp_name_t na; !%pp_constr c; !%pp_constr t]
)
let pp_rel_context = pp_list pp_rel_declaration
let pp_declaration_arity (pp_a, pp_b) = pp_variant (function
| RegularArity a -> "RegularArity", [!%pp_a a]
| TemplateArity b -> "TemplateArity", [!%pp_b b]
)
let pp_types = pp_constr
let pp_regular_inductive_arity = pp_record (fun ria -> [
  "mind_user_arity", !%pp_types ria.mind_user_arity;
  "mind_sort",       !%pp_sorts_t ria.mind_sort;
])
let pp_universe_level = pp_of Univ.Level.pr 
let pp_template_arity = pp_record (fun ta -> [
  "template_param_levels",
  !%(pp_list (pp_option pp_universe_level)) ta.template_param_levels;
  "template_level", !%pp_universe ta.template_level
])
let pp_inductive_arity =
  pp_declaration_arity (pp_regular_inductive_arity, pp_template_arity)
let pp_sorts_family = pp_variant (function
| InProp -> "InProp", []
| InSet -> "InSet", []
| InType -> "InType", []
)
let pp_rtree_t pp_a = pp_of (Rtree.pp_tree (pr_of pp_a))
let pp_recarg = pp_variant (function 
| Norec -> "Norec", []
| Mrec i -> "Mrec", [!%pp_inductive i]
| Imbr i -> "Imbr", [!%pp_inductive i]
)
let pp_wf_paths = pp_rtree_t pp_recarg
let pp_tag = pp_int
let pp_reloc_table = pp_array (pp_tuple_2 (pp_tag, pp_int))
let pp_one_inductive_body = pp_record (fun oib -> [
  "mind_typename",       !%pp_id_t oib.mind_typename;
  "mind_arity_ctxt",     !%pp_rel_context oib.mind_arity_ctxt;
  "mind_arity",          !%pp_inductive_arity oib.mind_arity;
  "mind_consnames",      !%(pp_array pp_id_t) oib.mind_consnames;
  "mind_user_lc",        !%(pp_array pp_types) oib.mind_user_lc;
  "mind_nrealargs",      !%pp_int oib.mind_nrealargs;
  "mind_nrealdecls",     !%pp_int oib.mind_nrealdecls;
  "mind_kelim",          !%(pp_list pp_sorts_family) oib.mind_kelim;
  "mind_nf_lc",          !%(pp_array pp_types) oib.mind_nf_lc;
  "mind_consnrealargs",  !%(pp_array pp_int) oib.mind_consnrealargs;
  "mind_consnrealdecls", !%(pp_array pp_int) oib.mind_consnrealdecls;
  "mind_recargs",        !%pp_wf_paths oib.mind_recargs;
  "mind_nb_constant",    !%pp_int oib.mind_nb_constant;
  "mind_nb_args",        !%pp_int oib.mind_nb_args;
  "mind_reloc_tbl",      !%pp_reloc_table oib.mind_reloc_tbl;
])
let pp_mutual_inductive = pp_mut_ind_t
let pp_projection_body = pp_record (fun pb -> [
  "proj_ind", !%pp_mutual_inductive pb.proj_ind;
  "proj_npars", !%pp_int pb.proj_npars;
  "proj_arg",  !%pp_int pb.proj_arg;
  "proj_type",  !%pp_types pb.proj_type;
  "proj_eta",  !%(pp_tuple_2 (pp_constr, pp_types)) pb.proj_eta;
  "proj_body", !%pp_constr pb.proj_body;
])
let pp_record_body =
  pp_option (pp_tuple_3 (pp_id_t, pp_array pp_constant, pp_array pp_projection_body))
let pp_recursivity_kind = pp_variant Decl_kinds.(function
| Finite -> "Finite", []
| CoFinite -> "CoFinite", []
| BiFinite -> "BiFinite", []
)
let pp_universe_context = pp_of (Univ.pr_universe_context Univ.Level.pr)
let pp_universe_context = !!"universe_context"
let pp_mutual_inductive_body = pp_record (fun mib -> [
  "mind_packets",     !%(pp_array pp_one_inductive_body) mib.mind_packets;
  "mind_record",      !%(pp_option pp_record_body) mib.mind_record;
  "mind_finite",      !%pp_recursivity_kind mib.mind_finite;
  "mind_ntypes",      !%pp_int mib.mind_ntypes;
  "mind_hyps",        !%pp_section_context mib.mind_hyps;
  "mind_nparams",     !%pp_int mib.mind_nparams;
  "mind_nparams_rec", !%pp_int mib.mind_nparams_rec;
  "mind_params_ctxt", !%pp_rel_context mib.mind_params_ctxt;
  "mind_polymorphic", !%pp_bool mib.mind_polymorphic;
  "mind_universes",   !%pp_universe_context mib.mind_universes;
  "mind_private",     !%(pp_option pp_bool) mib.mind_private;
])
let pp_mpmap_key prf mp = pp_string prf (ModPath.to_string mp)
let pp_mpmap_t pp_a prf mm =
  pp_list (pp_tuple_2 (pp_mpmap_key, pp_a)) prf (MPmap.bindings mm)
let pp_dir_path_t prf dp = pp_string prf (DirPath.to_string dp)
let pp_mbid_t prf mbi = pp_string prf (MBId.to_string mbi)
let pp_label_t = pp_of Label.print
let rec pp_module_path prf = pp_variant (function
| MPfile dp -> "MPfile", [!%pp_dir_path_t dp]
| MPbound mbi -> "MPbound", [!%pp_mbid_t mbi]
| MPdot(mp,l) -> "MPdot", [!%pp_module_path mp; !%pp_label_t l]
) prf
let rec pp_functorize (pp_ty, pp_a) = pp_variant (function
| NoFunctor a -> "NoFunctor", [!%pp_a a]
| MoreFunctor(mbi,ty,f) ->
   "MoreFunctor", [!%pp_mbid_t mbi; !%pp_ty ty; !%(pp_functorize (pp_ty, pp_a)) f]
)
let pp_in_universe_context pp_a = pp_tuple_2 (pp_a, pp_universe_context)
let pp_with_declaration = pp_variant (function
| WithMod(is,mp) -> "WithMod", [!%(pp_list pp_id_t) is; !%pp_module_path mp]
| WithDef(is,cc) ->
   "WithDef", [!%(pp_list pp_id_t) is; !%(pp_in_universe_context pp_constr) cc]
)
let rec pp_module_alg_expr prf = pp_variant (function
| MEident mp -> "MEident", [!%pp_module_path mp]
| MEapply(mae,mp) -> "MEapply", [!%pp_module_alg_expr mae; !%pp_module_path mp]
| MEwith(mae,wd) -> "MEwith", [!%pp_module_alg_expr mae; !%pp_with_declaration wd]
) prf
let pp_context_set_t = pp_of (Univ.pr_universe_context_set Univ.Level.pr)
let pp_context_set_t = !!"Univ.ContextSet.t"
let pp_delta_resolver = pp_of Mod_subst.debug_pr_delta
let pp_inline = pp_option pp_int
let pp_substitution = pp_of Mod_subst.debug_pr_subst
let pp_substituted pp_a prf s =
  pp_tuple_2 (pp_option (pp_list pp_substitution), pp_a) prf
             (Mod_subst.repr_substituted s)
let pp_opaque = ??"Opaqueproof.opaque"
let pp_constant_def = pp_variant (function
| Undef i -> "Undef", [!%pp_inline i]
| Def cs -> "Def", [!%(pp_substituted pp_constr) cs]
| OpaqueDef o -> "OpaqueDef", [!%pp_opaque o]
)
let pp_constant_type = 
  pp_declaration_arity (pp_types, pp_tuple_2 (pp_rel_context, pp_template_arity)) 
let pp_emitcode = ??"emitcode"
let pp_patch = ??"patch"
let pp_fv = ??"fv"
let pp_to_patch = pp_tuple_3 (pp_emitcode, pp_list pp_patch, pp_fv)
let pp_body_code = pp_variant Cemitcodes.(function
| BCdefined tp -> "BCdefined", [!%pp_to_patch tp];
| BCalias c -> "BCalias", [!%pp_constant c]
| BCconstant -> "BCconstant", []
)
let pp_to_patch_substituted prf tps =
  pp_tuple_2 (pp_option (pp_list pp_substitution), pp_body_code) prf
             (Cemitcodes.repr_body_code tps)
let pp_constant_universes = ??"constant_universes"
let pp_constant_body = pp_record (fun cb -> [
  "const_hyps",        !%pp_section_context cb.const_hyps;
  "const_body",        !%pp_constant_def cb.const_body;
  "const_type",        !%pp_constant_type cb.const_type;
  "const_body_code",   !%(pp_option pp_to_patch_substituted) cb.const_body_code;
  "const_polymorphic", !%pp_bool cb.const_polymorphic;
  "const_universes",   !%pp_constant_universes cb.const_universes;
  "const_proj",        !%(pp_option pp_projection_body) cb.const_proj;
  "const_inline_code", !%pp_bool cb.const_inline_code;
])
let rec pp_structure_field_body prf = pp_variant (function
| SFBconst cb -> "SFBconst", [!%pp_constant_body cb]
| SFBmind mib -> "SFBmind", [!%pp_mutual_inductive_body mib]
| SFBmodule mb -> "SFBmodule", [!%pp_module_body mb]
| SFBmodtype mtb -> "SFBmodtype", [!%pp_module_type_body mtb]
) prf
and pp_structure_body prf = pp_list (pp_tuple_2 (pp_label_t, pp_structure_field_body))
and pp_module_signature prf = ??"module_signature" prf
and pp_module_expression prf = 
  pp_functorize (pp_module_type_body,pp_module_alg_expr) prf
and pp_module_implementation prf = pp_variant (function
| Abstract -> "Abstract", []
| Algebraic me -> "Algebraic", [!%pp_module_expression me]
| Struct ms -> "Struct", [!%pp_module_signature ms]
| FullStruct -> "FullStruct", []
) prf
and pp_module_body prf = pp_record (fun mb -> [ 
  "mod_mp", !%pp_module_path mb.mod_mp;
  "mod_expr", !%pp_module_implementation mb.mod_expr;
  "mod_type", !%pp_module_signature mb.mod_type;
  "mod_type_alg", !%(pp_option pp_module_expression) mb.mod_type_alg;
  "mod_constraints", !%pp_context_set_t mb.mod_constraints;
  "mod_delta",    !%pp_delta_resolver mb.mod_delta;
  "mod_retroknowledge", !%(pp_list pp_retroknowledge_action) mb.mod_retroknowledge;
]) prf
and pp_module_type_body prf = pp_module_body prf

open Pre_env
let pp_link_info = pp_variant (function 
  | Linked s -> "Linked", [!%pp_string s]
  | LinkedInteractive s -> "LinkedInteractive", [!%pp_string s]
  | NotLinked -> "NotLinked", []
)
let pp_mind_key = pp_tuple_2 (pp_mutual_inductive_body, pp_ref pp_link_info)
let pp_constant_key =
  pp_tuple_2 (pp_constant_body,
              pp_tuple_2 (pp_ref pp_link_info,
                          pp_ref (pp_option (pp_ephemeron_key pp_int))))
let pp_cmap_env_key prf c = pp_string prf (Constant.to_string c)
let pp_cmap_env_t pp_a prf e = 
  pp_list (pp_tuple_2 (pp_cmap_env_key, pp_a)) prf (Cmap_env.bindings e)
let pp_mindmap_env_key = pp_mut_ind_t
let pp_mindmap_env_t pp_a prf me =
  pp_list (pp_tuple_2 (pp_mindmap_env_key, pp_a)) prf (Mindmap_env.bindings me)

let pp_globals = pp_record (fun g -> [
  "env_constants", !%(pp_cmap_env_t pp_constant_key) g.env_constants;
  "env_inductives", !%(pp_mindmap_env_t pp_mind_key)  g.env_inductives;
  "env_modules",    !%(pp_mpmap_t pp_module_body) g.env_modules;
  "env_modtypes",   !%(pp_mpmap_t pp_module_type_body) g.env_modtypes;
])
let pp_named_context = pp_list pp_named_declaration
let pp_lazy_val = ??"lazy_val"
let pp_named_vals = pp_list (pp_tuple_2 (pp_id_t, pp_lazy_val))
(*let pp_universes = pp_of (Univ.pr_universes Univ.Level.pr) (* too many *)*)
let pp_universes = !!"universes"
let pp_set_predicativity = pp_variant (function
| ImpredicativeSet -> "ImpredicativeSet", []
| PredicativeSet -> "PredicativeSet", []
)
let pp_engagement = pp_set_predicativity
let pp_stratification = pp_record (fun s -> [
  "env_universes", !%pp_universes s.env_universes;
  "env_engagement", !%pp_engagement s.env_engagement;
])
let pp_conv_oracle = ??"conv_oracle" (* strategy? *)
let pp_retroknowledge = ??"retroknowledge" (* optimization? *)
let pp_int_map pp_a prf im = pp_list (pp_tuple_2 (pp_int,pp_a)) prf (Int.Map.bindings im)
let pp_cooking_info prf ci = ??"cooking_info" prf ci
(* let pp_proofterm prf pt = ??"proofterm" prf pt *)
let pp_future_computation pp_a = pp_of (Future.print (pr_of pp_a))
let pp_universe_context_set = pp_context_set_t
let pp_uuid_t prf (i:Future.UUID.t) = pp_int prf (Obj.magic i)
let pp_future_uuid_map_t pp_a prf fum =
  pp_list (pp_tuple_2 (pp_uuid_t, pp_a)) prf (Future.UUIDMap.bindings fum)
let pp_opaquetab prf ot =
  (* pp_tuple_2 (pp_int_map (pp_tuple_2 (pp_list pp_cooking_info, pp_proofterm)),
                 pp_dir_path_t) *)
  pp_tuple_4 (pp_array (pp_future_computation pp_constr),
              pp_array (pp_future_computation pp_universe_context_set),
              pp_array (pp_list pp_cooking_info),
              pp_future_uuid_map_t pp_int) prf (Opaqueproof.dump ot)


let pp_pre_env = pp_record (fun e -> [
  "env_globals",        !%pp_globals e.env_globals;
  "env_named_context",  !%pp_named_context e.env_named_context.env_named_ctx;
  "env_rel_context",    !%pp_rel_context e.env_rel_context;
  "env_rel_val",        !%(pp_list pp_lazy_val) e.env_rel_val;
  "env_nb_rel",         !%pp_int e.env_nb_rel;
  "env_stratification", !%pp_stratification e.env_stratification;
  "env_conv_oracle",    !%pp_conv_oracle e.env_conv_oracle;
  "retroknowledge",     !%pp_retroknowledge e.retroknowledge;
  "indirect_pterms",    !%pp_opaquetab e.indirect_pterms;
])

let pp_env prf e = pp_pre_env prf (Environ.pre_env e)

(* Evd *)
open Evd
let pp_named_context_val = ??"named_context_val"
let pp_evar_body = pp_variant (function
  | Evar_empty -> "Evar_empty", []
  | Evar_defined c -> "Evar_defined", [!%pp_constr c]
)
let pp_filter_t prf f = pp_option (pp_list pp_bool) prf (Filter.repr f)
let pp_located pp_a = ??"located"
let pp_variable = pp_id_t
let pp_global_reference = pp_variant Globnames.(function
| VarRef v -> "VarRef", [!%pp_variable v]
| ConstRef c -> "ConstRef", [!%pp_constant c]
| IndRef i -> "IndRef", [!%pp_inductive i]
| ConstructRef c -> "ConstructRef", [!%pp_constructor c]
)
let pp_obligation_definition_status = pp_variant Evar_kinds.(function
| Define b -> "Define", [!%pp_bool b]
| Expand -> "Expand", []
)
let pp_evar_kinds_t = pp_variant Evar_kinds.(function
| ImplicitArg(gr,ii,b) -> "ImplicitArg", [!%pp_global_reference gr;
                                          !%(pp_tuple_2 (pp_int, pp_option pp_id_t)) ii;
                                          !%pp_bool b]
| BinderType n -> "BinderTypeof", [!%pp_name_t n]
| QuestionMark ods -> "QuestionMark", [!%pp_obligation_definition_status ods]
| CasesType b -> "CasesType", [!%pp_bool b]
| InternalHole -> "InternalHole", []
| TomatchTypeParameter(ind,i) -> "TomatchTypeParameter", [!%pp_inductive ind; !%pp_int i]
| GoalEvar -> "GoalEvar", []
| ImpossibleCase -> "ImpossibleCase", []
| MatchingVar(b,i) -> "MatchingVar", [!%pp_bool b; !%pp_id_t i]
| VarInstance i -> "VarInstance", [!%pp_id_t i]
| SubEvar k -> "SubEvar", [!%pp_existential_key k]
)
let pp_store_t = ??"store_t"
let pp_evar_info = pp_record (function ei -> [ 
  "evar_hyps",       !%pp_named_context_val ei.evar_hyps;
  "evar_concl",      !%pp_constr ei.evar_concl;
  "evar_body",       !%pp_evar_body ei.evar_body;
  "evar_filter",     !%pp_filter_t ei.evar_filter;
  "evar_source",     !%(pp_located pp_evar_kinds_t) ei.evar_source;
  "evar_candidates", !%(pp_option (pp_list pp_constr)) ei.evar_candidates;
  "evar_extra",      !%pp_store_t ei.evar_extra;
])
let pp_evar_map prf em =
  pp_list (pp_tuple_2 (pp_evar_t, pp_evar_info)) prf
          (Evd.fold (fun ev ei l -> (ev,ei)::l) em [])

let pp_goal = pp_evar_t
let pp_proof prf p =
  pp_tuple_5 (pp_list pp_goal, (* focused goals *)
              pp_list (pp_tuple_2 (pp_list pp_goal, pp_list pp_goal)), (* background goals *)
              pp_list pp_goal, (* shelved goals *)
              pp_list pp_goal, (* given up goald *)
              pp_evar_map)     (* underlying evar_map *)
    prf (Proof.proof p)
let pp_pre_goals pp_a = pp_record Proof.(fun pg -> [
  "fg_goals", !%(pp_list pp_a) pg.fg_goals;
  "bg_goals", !%(pp_list (pp_tuple_2 (pp_list pp_a, pp_list pp_a))) pg.bg_goals;
  "shelved_goals", !%(pp_list pp_a) pg.shelved_goals;
  "given_up_goals", !%(pp_list pp_a) pg.given_up_goals;
])


let print_constr c = printf "%a@." pp_constr c
let print_env e = printf "%a@." pp_env e
let print_evar_info ei = printf "%a@." pp_evar_info ei
let print_evar_map em = printf "%a@." pp_evar_map em
let print_proof p = printf "%a@." pp_proof p
let print_sorts p = printf "%a@." pp_sorts_t p
let print_lconstr c =
  printf "%a@.    ||@.%s@." pp_constr c (Pp.string_of_ppcmds (Printer.pr_lconstr c))

let print_pure_constr = print_constr
