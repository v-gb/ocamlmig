open Base
open! Common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open! Fmast

let dummy_lid : Longident.t = Lident ""
let dummy_loc = Location.none
let dummy_stringloc : _ Location.loc = { txt = ""; loc = dummy_loc }
let dummy_lidloc : _ Location.loc = { txt = dummy_lid; loc = dummy_loc }
let dummy_expr = Ast_helper.Exp.hole ()
let dummy_pat = Ast_helper.Pat.any ()
let dummy_stri = Ast_helper.Str.eval dummy_expr
let dummy_str = []
let dummy_typ = Ast_helper.Typ.any ()
let dummy_payload : P.payload = PStr []
let dummy_ext : P.extension = (dummy_stringloc, dummy_payload)
let dummy_class_field = Ast_helper.Cf.extension dummy_ext
let dummy_class_type = Ast_helper.Cty.extension dummy_ext

let shallow_equality =
  let super = Ast_mapper.default_mapper in
  let self1 =
    { super with
      expr =
        (fun self v ->
          super.expr self
            (match v.pexp_desc with
            | Pexp_construct (_, (Some _ as arg)) ->
                { v with pexp_desc = Pexp_construct (dummy_lidloc, arg) }
            | _ -> v))
    ; pat =
        (fun self v ->
          super.pat self
            (match v.ppat_desc with
            | Ppat_construct (_, (Some _ as arg)) ->
                { v with ppat_desc = Ppat_construct (dummy_lidloc, arg) }
            | _ -> v))
    }
  in
  let self2 =
    { super with
      expr = (fun _ _ -> dummy_expr)
    ; location = (fun _ _ -> dummy_loc)
    ; pat = (fun _ _ -> dummy_pat)
    ; structure_item = (fun _ _ -> dummy_stri)
    ; structure = (fun _ _ -> dummy_str)
    ; typ = (fun _ _ -> dummy_typ)
    ; class_field = (fun _ _ -> dummy_class_field)
    ; class_type = (fun _ _ -> dummy_class_type)
    }
  in
  fun meth v1 v2 ->
    (* Crucially, we call self1 here, not self2, so we hide children, but not the root
       node. *)
    Poly.( = ) ((meth self1) self2 v1) ((meth self1) self2 v2)

let children (ctx : Ocamlformat_lib.Ast.t) meth v =
  let children : (Ocamlformat_lib.Ast.t * _) list ref = ref [] in
  let super = Ast_mapper.default_mapper in
  let ctx = ref ctx in
  let self1 =
    { super with
      expr =
        (fun self v ->
          (match v.pexp_desc with
          | Pexp_construct (id, Some _) ->
              (* This allows diffing the constructor name separately from the
                   payload, which is useful because open/unopen etc will modify
                   constructors, and it's unnecessary to reformat the whole payload
                   just for that.

                   We only do this with a payload, otherwise our made-up Pexp_construct
                   child would cause the ast to be infinite. *)
              let e =
                { P.pexp_desc = Pexp_construct (id, None)
                ; pexp_attributes = []
                ; pexp_loc = id.loc
                ; pexp_loc_stack = []
                }
              in
              children := (Exp (Ast_helper.Exp.tuple [ e ]), `Expr e) :: !children
          | _ -> ());
          super.expr self v)
    ; pat =
        (fun self v ->
          (match v.ppat_desc with
          | Ppat_construct (id, Some _) ->
              let p =
                { P.ppat_desc = Ppat_construct (id, None)
                ; ppat_attributes = []
                ; ppat_loc = id.loc
                ; ppat_loc_stack = []
                }
              in
              children := (Pat (Ast_helper.Pat.tuple [ p ]), `Pat p) :: !children
          | _ -> ());
          super.pat self v)
    }
  in
  (* We need to do something for every constructor in Ast.t, otherwise we can get
     assertion failures from the [assert_check_exp] kind of functions in ast.ml, due to
     trying to create [Ast.subexp ~ctx exp] with [exp] not being a child of [ctx]. We
     can either stop the traversal (like for expr), or update the "parent" node during
     the traversal of a subtree, as with payload. *)
  let _ : Ocamlformat_lib.Ast.t -> _ = function
    | Pld _ -> `Ref
    | Typ _ -> `Child
    | Td _ -> `Ref
    | Cty _ -> `Child
    | Cd _ -> `Ref
    | Ctd _ -> `Ref
    | Pat _ -> `Child
    | Exp _ -> `Child
    | Fpe _ -> `Ref
    | Fpc _ -> `Ref
    | Vc _ -> `Ref
    | Lb _ -> `Ref
    | Bo _ -> `Ref
    | Mb _ -> `Ref
    | Md _ -> `Ref
    | Cl _ -> `Ref
    | Mty _ -> `Ref
    | Mod _ -> `Ref
    | Sig _ -> `Ref
    | Str _ -> `Child
    | Clf _ -> `Child
    | Ctf _ -> `Ref
    | Tli _ -> `Dont_care_I_think
    | Top -> `Dont_care_I_think
    | Rep -> `Dont_care_I_think
  in
  let self2 =
    { super with
      expr =
        (fun _ v ->
          children := (!ctx, `Expr v) :: !children;
          v)
    ; pat =
        (fun _ v ->
          children := (!ctx, `Pat v) :: !children;
          v)
    ; structure_item =
        (fun _ v ->
          children := (!ctx, `Stri v) :: !children;
          v)
    ; structure =
        (fun _ v ->
          children := (!ctx, `Str v) :: !children;
          v)
    ; typ =
        (fun _ v ->
          children := (!ctx, `Typ v) :: !children;
          v)
    ; class_field =
        (fun _ v ->
          children := (!ctx, `Cf v) :: !children;
          v)
    ; class_type =
        (fun _ v ->
          children := (!ctx, `Cty v) :: !children;
          v)
    ; payload =
        (fun self v ->
          Ref.set_temporarily ctx (Pld v) ~f:(fun () -> super.payload self v))
    ; type_declaration =
        (fun self v ->
          Ref.set_temporarily ctx (Td v) ~f:(fun () -> super.type_declaration self v))
    ; class_declaration =
        (fun self v ->
          Ref.set_temporarily ctx (Cd v) ~f:(fun () -> super.class_declaration self v))
    ; class_type_declaration =
        (fun self v ->
          Ref.set_temporarily ctx (Ctd v) ~f:(fun () ->
              super.class_type_declaration self v))
    ; expr_function_param =
        (fun self v ->
          Ref.set_temporarily ctx (Fpe v) ~f:(fun () -> super.expr_function_param self v))
    ; class_function_param =
        (fun self v ->
          Ref.set_temporarily ctx (Fpc v) ~f:(fun () -> super.class_function_param self v))
    ; value_constraint =
        (fun self v ->
          Ref.set_temporarily ctx (Vc v) ~f:(fun () -> super.value_constraint self v))
    ; value_binding =
        (fun self v ->
          Ref.set_temporarily ctx (Lb v) ~f:(fun () -> super.value_binding self v))
    ; binding_op =
        (fun self v ->
          Ref.set_temporarily ctx (Bo v) ~f:(fun () -> super.binding_op self v))
    ; module_binding =
        (fun self v ->
          Ref.set_temporarily ctx (Mb v) ~f:(fun () -> super.module_binding self v))
    ; module_declaration =
        (fun self v ->
          Ref.set_temporarily ctx (Md v) ~f:(fun () -> super.module_declaration self v))
    ; class_expr =
        (fun self v ->
          Ref.set_temporarily ctx (Cl v) ~f:(fun () -> super.class_expr self v))
    ; module_type =
        (fun self v ->
          Ref.set_temporarily ctx (Mty v) ~f:(fun () -> super.module_type self v))
    ; module_expr =
        (fun self v ->
          Ref.set_temporarily ctx (Mod v) ~f:(fun () -> super.module_expr self v))
    ; signature_item =
        (fun self v ->
          Ref.set_temporarily ctx (Sig v) ~f:(fun () -> super.signature_item self v))
    ; class_type_field =
        (fun self v ->
          Ref.set_temporarily ctx (Ctf v) ~f:(fun () -> super.class_type_field self v))
    }
  in
  ignore ((meth self1) self2 v);
  !children

module Ast = Ocamlformat_lib.Ast

type 'a xt = 'a Ast.xt

let sexp_of_xt sexp_of_a (t : _ xt) = sexp_of_a t.ast

type diff =
  [ `Expr of expression * expression xt
  | `Pat of pattern * pattern xt
  | `Str of structure * structure
  | `Stri of structure_item * structure_item xt
  | `Typ of core_type * core_type xt
  | `Cf of class_field * class_field xt
  | `Cty of class_type * class_type xt
  ]
[@@deriving sexp_of]

type diff_out =
  [ diff
  | `Rem of Location.t
  ]
[@@deriving sexp_of]

let list_diff l1 l2 loc_of =
  (* In unopen, we remove [open Foo] from structures, causing the right side to be
     shorter. To try to line up the two sides, we assume that positions are increasing
     (which is likely the case, because no/few transformations would currently touch
     structure items), and because we try very hard to have sensible location even in
     added code, since it majorly impact comment placement. *)
  let rec combine diff acc l1 l2 =
    match (l1, l2) with
    | [], l2 -> (List.rev diff @ List.map l2 ~f:(`Add __), List.rev acc)
    | l1, [] -> (List.rev diff @ List.map l1 ~f:(`Rem __), List.rev acc)
    | x1 :: tl1, x2 :: tl2 ->
        let c = Location.Ignoring_filename.compare (loc_of x1) (loc_of x2) in
        if c = 0
        then combine diff ((x1, x2) :: acc) tl1 tl2
        else if c < 0
        then combine (`Rem x1 :: diff) acc tl1 l2
        else combine (`Add x2 :: diff) acc l1 tl2
  in
  combine [] [] l1 l2

let rec diff2 (vs : diff) (ctx : Ast.t) (f : diff_out -> unit) =
  match vs with
  | `Expr (v1, v2) -> diff2_meth __.expr vs v1 v2.ast ctx f
  | `Pat (v1, v2) -> diff2_meth __.pat vs v1 v2.ast ctx f
  | `Stri (v1, v2) -> diff2_meth __.structure_item vs v1 v2.ast ctx f
  | `Str (v1, v2) ->
      if List.length v1 <> List.length v2
      then (
        let diff, common = list_diff v1 v2 __.pstr_loc in
        List.iter diff ~f:(function
          | `Rem stri -> f (`Rem stri.pstr_loc)
          | `Add _ -> failwith "`Add unimplemented");
        let v1, v2 = List.unzip common in
        diff2_meth __.structure vs v1 v2 ctx f)
      else diff2_meth __.structure vs v1 v2 ctx f
  | `Typ (v1, v2) -> diff2_meth __.typ vs v1 v2.ast ctx f
  | `Cf (v1, v2) -> diff2_meth __.class_field vs v1 v2.ast ctx f
  | `Cty (v1, v2) -> diff2_meth __.class_type vs v1 v2.ast ctx f

and diff2_meth : type a.
    (Ast_mapper.mapper -> Ast_mapper.mapper -> a -> a) -> _ -> a -> a -> _ =
 fun meth vs v1 v2 ctx f ->
  if shallow_equality meth v1 v2
  then
    List.iter2_exn (children Top meth v1) (children ctx meth v2)
      ~f:(fun (_, c1) (ctx, c2) ->
        match (c1, c2) with
        | `Expr v1, `Expr v2 -> diff2 (`Expr (v1, Ast.sub_exp ~ctx v2)) (Exp v2) f
        | `Pat v1, `Pat v2 -> diff2 (`Pat (v1, Ast.sub_pat ~ctx v2)) (Pat v2) f
        | `Stri v1, `Stri v2 -> diff2 (`Stri (v1, Ast.sub_str ~ctx v2)) (Str v2) f
        | `Str v1, `Str v2 -> diff2 (`Str (v1, v2)) Top f
        | `Typ v1, `Typ v2 -> diff2 (`Typ (v1, Ast.sub_typ ~ctx v2)) (Typ v2) f
        | `Cf v1, `Cf v2 -> diff2 (`Cf (v1, Ast.sub_cf ~ctx v2)) (Clf v2) f
        | `Cty v1, `Cty v2 -> diff2 (`Cty (v1, Ast.sub_cty ~ctx v2)) (Cty v2) f
        | (`Expr _ | `Pat _ | `Stri _ | `Str _ | `Typ _ | `Cf _ | `Cty _), _ ->
            assert false)
  else f (vs : diff :> diff_out)

(* Limiting the type, because we can't use Top as a context for any random fragment of
   ast, so we'd need to do something smarter like Fmt_ast does. In fact, if we needed
   to diff more types, maybe we should write a function that creates a context for any
   extended ast, and use it in Fmt_ast, and potentially here. *)
let diff2 str1 str2 = diff2 (`Str (str1, str2)) Top

type add_comments =
  { add_comments :
      'a. 'a -> Location.t -> 'a Ocamlformat_lib.Parse_with_comments.with_comments
  }

let indexed_comments
    (ast_with_comments : _ Ocamlformat_lib.Parse_with_comments.with_comments) =
  let map =
    List.map ast_with_comments.comments ~f:(fun comment ->
        ((Ocamlformat_lib.Cmt.loc comment).loc_start.pos_cnum, comment))
    |> Map.of_alist_multi (module Int)
  in
  { add_comments =
      (fun ast loc ->
        { ast_with_comments with
          ast
        ; comments =
            Map.subrange map ~lower_bound:(Incl loc.loc_start.pos_cnum)
              ~upper_bound:(Incl loc.loc_end.pos_cnum)
            |> Map.data
            |> List.concat
        })
  }

let printed_ast add_comments (loc : Location.t) ext ast =
  let target_margin = (* should infer from source file *) 90 in
  let current_indentation = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let fmconf =
    Ocamlformat_lib.Conf.default
    |> Ocamlformat_lib.Conf.update_value ~name:"margin"
         ~value:(Int.to_string (Int.max 10 (target_margin - current_indentation)))
    |> function
    | Ok v -> v
    | Error e -> failwith (Ocamlformat_lib.Conf_t.Error.to_string e)
  in
  ocamlformat_print ext ~conf:fmconf (add_comments.add_comments ast loc)
  |> String.chop_suffix_if_exists ~suffix:"\n"
  |> String.split ~on:'\n'
  |> List.mapi ~f:(fun i s ->
         if i = 0 then s else String.make current_indentation ' ' ^ s)
  |> String.concat ~sep:"\n"

let minprint ~debug_diff ~source_contents ~structure ~structure' =
  let buf = Buffer.create (String.length source_contents) in
  let add_comments = indexed_comments structure in
  let pos = ref 0 in
  let copy_orig to_ =
    Buffer.add_substring buf source_contents ~pos:!pos ~len:(to_ - !pos);
    pos := to_
  in
  let loc_of_diff : diff_out -> _ = function
    | `Expr (e, _) -> e.pexp_loc
    | `Pat (p, _) -> p.ppat_loc
    | `Stri (si, _) -> si.pstr_loc
    | `Str _ -> assert false
    | `Typ (t, _) -> t.ptyp_loc
    | `Cf (v, _) -> v.pcf_loc
    | `Cty (v, _) -> v.pcty_loc
    | `Rem loc -> loc
  in
  let l =
    let r = ref [] in
    diff2 structure.ast structure' (fun x -> r := x :: !r);
    List.rev !r
    |> List.sort ~compare:(fun a b -> Location.compare (loc_of_diff a) (loc_of_diff b))
  in
  let prev = ref None in
  List.iter l ~f:(fun x ->
      Option.iter !prev ~f:(fun prev ->
          if (loc_of_diff prev).loc_end.pos_cnum > (loc_of_diff x).loc_start.pos_cnum
          then
            raise_s
              [%sexp
                "unexpected overlap in diff"
              , (prev : diff_out)
              , (loc_of_diff prev : Location.t)
              , (x : diff_out)
              , (loc_of_diff x : Location.t)]);
      prev := Some x);
  (* ideally, we'd pass the context into the printing function, so ocamlformat can
     print parens nicely, instead of this hack *)
  let parens_if b str = if b then "(" ^ str ^ ")" else str in
  List.iter l ~f:(fun diff ->
      let loc, str =
        match diff with
        | `Expr (e1, e2) ->
            ( e1.pexp_loc
            , parens_if (Ast.parenze_exp e2)
                (printed_ast add_comments e1.pexp_loc Expression e2.ast) )
        | `Pat (p1, p2) ->
            ( p1.ppat_loc
            , parens_if (Ast.parenze_pat p2)
                (printed_ast add_comments p1.ppat_loc Pattern p2.ast) )
        | `Stri (s1, s2) ->
            (s1.pstr_loc, printed_ast add_comments s1.pstr_loc Structure [ s2.ast ])
        | `Str _ -> assert false
        | `Typ (t1, t2) ->
            ( t1.ptyp_loc
            , parens_if (Ast.parenze_typ t2)
                (printed_ast add_comments t1.ptyp_loc Core_type t2.ast) )
        | `Cf (v1, v2) ->
            (v1.pcf_loc, printed_ast add_comments v1.pcf_loc Class_field v2.ast)
        | `Cty (v1, v2) ->
            ( v1.pcty_loc
            , parens_if (Ast.parenze_cty v2)
                (printed_ast add_comments v1.pcty_loc Class_type v2.ast) )
        | `Rem loc -> (loc, "")
      in
      copy_orig loc.loc_start.pos_cnum;
      if debug_diff
      then (
        Buffer.add_string buf "[34m[[31m";
        copy_orig loc.loc_end.pos_cnum;
        Buffer.add_string buf "[32m");
      Buffer.add_string buf str;
      if debug_diff then Buffer.add_string buf "[34m][39m";
      pos := loc.loc_end.pos_cnum);
  copy_orig (String.length source_contents);
  Buffer.contents buf

(* problems:
   - should avoid assert failure on additions of structure items
   - record fields should be special cased like variants
 *)
