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
let dummy_payload : P.payload = PStr []
let dummy_ext : P.extension = (dummy_stringloc, dummy_payload)
let dummy_expr = Ast_helper.Exp.hole ()
let dummy_pat = Ast_helper.Pat.any ()
let dummy_stri = Ast_helper.Str.eval dummy_expr
let dummy_sigi = Ast_helper.Sig.extension dummy_ext
let dummy_me = Ast_helper.Mod.ident dummy_lidloc
let dummy_typ = Ast_helper.Typ.any ()
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
      location =
        (fun _ _ -> dummy_loc)
        (* these methods should be in sync with the ones in [children] *)
    ; expr = (fun _ _ -> dummy_expr)
    ; pat = (fun _ _ -> dummy_pat)
    ; structure_item = (fun _ _ -> dummy_stri)
    ; signature_item = (fun _ _ -> dummy_sigi)
    ; module_expr = (fun _ _ -> dummy_me)
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
    | Mod _ -> `Child
    | Sig _ -> `Child
    | Str _ -> `Child
    | Clf _ -> `Child
    | Ctf _ -> `Ref
    | Tli _ -> `Dont_care_I_think
    | Top -> `Dont_care_I_think
    | Rep -> `Dont_care_I_think
  in
  let self2 =
    { super with
      (* these methods should be in sync with the ones in [shallow_equality] *)
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
    ; signature_item =
        (fun _ v ->
          children := (!ctx, `Sigi v) :: !children;
          v)
    ; module_expr =
        (fun _ v ->
          children := (!ctx, `Me v) :: !children;
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
        (* *)
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
  | `Me of module_expr * module_expr xt
  | `Stri of structure_item * structure_item xt
  | `Sigi of signature_item * signature_item xt
  | `Typ of core_type * core_type xt
  | `Cf of class_field * class_field xt
  | `Cty of class_type * class_type xt
  ]
[@@deriving sexp_of]

type diff_out =
  [ diff
  | `Rem of Location.t
  | `Add_stri of Location.t * (int * structure_item)
  | `Add_sigi of Location.t * (int * signature_item)
  ]
[@@deriving sexp_of]

let list_diff l1 l2 loc_of =
  (* In unopen, we remove [open Foo] from structures, causing the right side to be
     shorter. To try to line up the two sides, we assume that positions are increasing
     (which is likely the case, because no/few transformations would currently touch
     structure items), and because we try very hard to have sensible location even in
     added code, since it majorly impact comment placement. *)
  let compare_loc loc1 loc2 =
    (* Items from the migration are lowest, so they are added as soon as possible by
       the merge below. *)
    [%compare: bool * Location.Ignoring_filename.t]
      (not (Transform_common.is_migrate_filename loc1), loc1)
      (not (Transform_common.is_migrate_filename loc2), loc2)
  in
  let rec combine diff acc l1 l2 prev1 =
    match (l1, l2) with
    | [], l2 ->
        (List.rev diff @ List.map l2 ~f:(fun x2 -> `Add (prev1, x2, None)), List.rev acc)
    | l1, [] -> (List.rev diff @ List.map l1 ~f:(`Rem __), List.rev acc)
    | x1 :: tl1, x2 :: tl2 ->
        let c = compare_loc (loc_of x1) (loc_of x2) in
        if c = 0
        then combine diff ((x1, x2) :: acc) tl1 tl2 (Some x1)
        else if c < 0
        then combine (`Rem x1 :: diff) acc tl1 l2 (Some x1)
        else combine (`Add (prev1, x2, Some x1) :: diff) acc l1 tl2 prev1
  in
  combine [] [] l1 l2 None

let rec diff2 ~source (vs : diff) (f : diff_out -> unit) =
  match vs with
  | `Expr (v1, v2) -> diff2_meth ~source __.expr vs v1 v2.ast (Exp v2.ast) f
  | `Pat (v1, v2) -> diff2_meth ~source __.pat vs v1 v2.ast (Pat v2.ast) f
  | `Stri (v1, v2) -> diff2_meth ~source __.structure_item vs v1 v2.ast (Str v2.ast) f
  | `Sigi (v1, v2) -> diff2_meth ~source __.signature_item vs v1 v2.ast (Sig v2.ast) f
  | `Me (v1, v2) -> (
      match (v1.pmod_desc, v2.ast.pmod_desc) with
      | Pmod_structure l1, Pmod_structure l2 when List.length l1 <> List.length l2 -> (
          match diff_str_sig ~file_type:Impl T ~source l1 l2 f with
          | `Ok -> ()
          | `Whole_structure ->
              diff2_meth ~source __.module_expr vs v1 v2.ast (Mod v2.ast) f)
      | _ -> diff2_meth ~source __.module_expr vs v1 v2.ast (Mod v2.ast) f)
  | `Typ (v1, v2) -> diff2_meth ~source __.typ vs v1 v2.ast (Typ v2.ast) f
  | `Cf (v1, v2) -> diff2_meth ~source __.class_field vs v1 v2.ast (Clf v2.ast) f
  | `Cty (v1, v2) -> diff2_meth ~source __.class_type vs v1 v2.ast (Cty v2.ast) f

and diff_str_sig : type a elt.
       file_type:a Transform_common.File_type.t
    -> (a, elt list) Type_equal.t
    -> source:string
    -> a
    -> a
    -> (diff_out -> unit)
    -> _ =
 fun ~file_type T ~source l1 l2 f ->
  let loc : elt -> Location.t =
    match file_type with Impl -> __.pstr_loc | Intf -> __.psig_loc
  in
  let diff, common =
    match List.zip l1 l2 with
    | Ok common -> ([], common)
    | Unequal_lengths -> list_diff l1 l2 loc
  in
  if
    List.exists diff ~f:(function
      | `Rem stri ->
          (* If the structure item is alone on its line, delete the whole line
            rather than leaving an empty line. *)
          let line_loc : Location.t =
            { loc_start =
                { (loc stri).loc_start with
                  pos_cnum =
                    Option.value ~default:0
                      (String.rindex_from source (loc stri).loc_start.pos_cnum '\n')
                }
            ; loc_end =
                { (loc stri).loc_end with
                  pos_cnum =
                    Option.value ~default:(String.length source)
                      (String.index_from source (loc stri).loc_end.pos_cnum '\n')
                }
            ; loc_ghost = false
            }
          in
          let loc_to_delete =
            if
              String.is_empty
                (String.strip
                   (string_sub source ~pos1:line_loc.loc_start.pos_cnum
                      ~pos2:(loc stri).loc_start.pos_cnum))
              && String.is_empty
                   (String.strip
                      (string_sub source ~pos1:(loc stri).loc_end.pos_cnum
                         ~pos2:line_loc.loc_end.pos_cnum))
            then line_loc
            else loc stri
          in
          f (`Rem loc_to_delete);
          false
      | `Add (_prev, stri, next) -> (
          match next with
          | None -> true
          | Some item ->
              (not
                 (String.is_empty
                    (String.strip
                       (string_sub source ~pos1:(loc item).loc_start.pos_bol
                          ~pos2:(loc item).loc_start.pos_cnum))))
              ||
              let indent = (loc item).loc_start.pos_cnum - (loc item).loc_start.pos_bol in
              let pos =
                { (loc item).loc_start with pos_cnum = (loc item).loc_start.pos_bol }
              in
              let loc : Location.t =
                { loc_start = pos; loc_end = pos; loc_ghost = false }
              in
              (match file_type with
              | Impl -> f (`Add_stri (loc, (indent, stri)))
              | Intf -> f (`Add_sigi (loc, (indent, stri))));
              false))
  then
    (* The problem here is: what position to use for the new structure item? We'd
         need to keep track of neighboring items. *)
    `Whole_structure
  else (
    List.iter common ~f:(fun (stri1, stri2) ->
        match file_type with
        | Impl -> diff2 ~source (`Stri (stri1, Ast.sub_str ~ctx:Top stri2)) f
        | Intf -> diff2 ~source (`Sigi (stri1, Ast.sub_sig ~ctx:Top stri2)) f);
    `Ok)

and diff2_meth : type a.
       source:_
    -> (Ast_mapper.mapper -> Ast_mapper.mapper -> a -> a)
    -> _
    -> a
    -> a
    -> Ast.t
    -> _ =
 fun ~source meth vs v1 v2 ctx f ->
  if shallow_equality meth v1 v2
  then
    List.iter2_exn (children Top meth v1) (children ctx meth v2)
      ~f:(fun (_, c1) (ctx, c2) ->
        match (c1, c2) with
        | `Expr v1, `Expr v2 -> diff2 ~source (`Expr (v1, Ast.sub_exp ~ctx v2)) f
        | `Pat v1, `Pat v2 -> diff2 ~source (`Pat (v1, Ast.sub_pat ~ctx v2)) f
        | `Stri v1, `Stri v2 -> diff2 ~source (`Stri (v1, Ast.sub_str ~ctx v2)) f
        | `Sigi v1, `Sigi v2 -> diff2 ~source (`Sigi (v1, Ast.sub_sig ~ctx v2)) f
        | `Me v1, `Me v2 -> diff2 ~source (`Me (v1, Ast.sub_mod ~ctx v2)) f
        | `Typ v1, `Typ v2 -> diff2 ~source (`Typ (v1, Ast.sub_typ ~ctx v2)) f
        | `Cf v1, `Cf v2 -> diff2 ~source (`Cf (v1, Ast.sub_cf ~ctx v2)) f
        | `Cty v1, `Cty v2 -> diff2 ~source (`Cty (v1, Ast.sub_cty ~ctx v2)) f
        | (`Expr _ | `Pat _ | `Stri _ | `Sigi _ | `Me _ | `Typ _ | `Cf _ | `Cty _), _ ->
            assert false)
  else f (vs : diff :> diff_out)

(* Limiting the type, because we can't use Top as a context for any random fragment of
   ast, so we'd need to do something smarter like Fmt_ast does. In fact, if we needed
   to diff more types, maybe we should write a function that creates a context for any
   extended ast, and use it in Fmt_ast, and potentially here. *)
let diff2 (type a) (file_type : a Transform_common.File_type.t)
    (str1 : a Ocamlformat_lib.Parse_with_comments.with_comments) (str2 : a) =
  let source : string = fst (Stdlib.Obj.magic str1.source) in
  assert (Stdlib.Obj.tag (Stdlib.Obj.repr source) = Stdlib.Obj.string_tag);
  let r = ref [] in
  match
    match file_type with
    | Impl ->
        diff_str_sig ~file_type T ~source str1.ast str2 (fun diff -> r := diff :: !r)
    | Intf ->
        diff_str_sig ~file_type T ~source str1.ast str2 (fun diff -> r := diff :: !r)
  with
  | `Ok -> `Ok (List.rev !r)
  | `Whole_structure -> `Whole_structure

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
              ~upper_bound:(Excl loc.loc_end.pos_cnum)
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

let tokens_would_fuse ~ocaml_version str1 str2 =
  let tokens str =
    let lexbuf = Lexing.from_string str in
    Ocamlformat_parser_extended.Lexer.init ~keyword_edition:(ocaml_version, []) ();
    let rec loop acc =
      match Ocamlformat_parser_extended.Lexer.token lexbuf with
      | exception (Ocamlformat_parser_extended.Lexer.Error _ as e) -> Error e
      | EOF -> Ok (List.rev acc)
      | token -> loop (token :: acc)
    in
    loop []
  in
  (* The sections of text we consider are cut at expressions/types/etc boundaries, so
     they are also token boundaries, so we don't need to lex from start of file. *)
  match (tokens str1, tokens (str1 ^ str2)) with
  | Error e, _ -> raise e
  | _, Error _ -> true
  | Ok tokens1, Ok tokens12 ->
      not
        (List.is_prefix ~prefix:tokens1 tokens12 ~equal:(Poly.equal : Parser.token -> _))

let stitch_code_together ~ocaml_version ~debug_diff orig f =
  let buf = Buffer.create (String.length orig) in
  let add_string =
    let last_chunk_of_data = ref "" in
    fun ~parsed str ->
      if parsed && not (String.is_empty str)
      then (
        if tokens_would_fuse ~ocaml_version !last_chunk_of_data str
        then (
          if debug_diff then Buffer.add_string buf "[42m";
          Buffer.add_string buf " ";
          if debug_diff then Buffer.add_string buf "[49m");
        last_chunk_of_data := str);
      Buffer.add_string buf str
  in
  let pos = ref 0 in
  let copy_orig ~parsed to_ =
    add_string ~parsed (string_sub orig ~pos1:!pos ~pos2:to_);
    pos := to_
  in
  f (fun (loc : Location.t) str ->
      copy_orig loc.loc_start.pos_cnum ~parsed:true;
      if debug_diff
      then (
        add_string "[34m[[31m" ~parsed:false;
        copy_orig loc.loc_end.pos_cnum ~parsed:false;
        add_string "[32m" ~parsed:false);
      add_string str ~parsed:true;
      if debug_diff then add_string "[34m][39m" ~parsed:false;
      pos := loc.loc_end.pos_cnum);
  copy_orig (String.length orig) ~parsed:true;
  Buffer.contents buf

let print ~ocaml_version ~debug_diff ~source_contents file_type ast1 ast2 =
  let add_comments = indexed_comments ast1 in
  let loc_of_diff : diff_out -> _ = function
    | `Expr (e, _) -> e.pexp_loc
    | `Pat (p, _) -> p.ppat_loc
    | `Stri (si, _) -> si.pstr_loc
    | `Sigi (si, _) -> si.psig_loc
    | `Me (me, _) -> me.pmod_loc
    | `Typ (t, _) -> t.ptyp_loc
    | `Cf (v, _) -> v.pcf_loc
    | `Cty (v, _) -> v.pcty_loc
    | `Rem loc -> loc
    | `Add_stri (loc, _) -> loc
    | `Add_sigi (loc, _) -> loc
  in
  match diff2 file_type ast1 ast2 with
  | `Whole_structure ->
      ocamlformat_print ~conf:Ocamlformat_lib.Conf.default
        (Transform_common.File_type.to_extended_ast file_type)
        { ast1 with ast = ast2 }
  | `Ok l ->
      let l =
        List.sort l ~compare:(fun a b -> Location.compare (loc_of_diff a) (loc_of_diff b))
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
      stitch_code_together ~ocaml_version ~debug_diff source_contents (fun f ->
          List.iter l ~f:(function
            | `Expr (e1, e2) ->
                f e1.pexp_loc
                  (parens_if (Ast.parenze_exp e2)
                     (printed_ast add_comments e1.pexp_loc Expression e2.ast))
            | `Pat (p1, p2) ->
                f p1.ppat_loc
                  (parens_if (Ast.parenze_pat p2)
                     (printed_ast add_comments p1.ppat_loc Pattern p2.ast))
            | `Stri (s1, s2) ->
                f s1.pstr_loc (printed_ast add_comments s1.pstr_loc Structure [ s2.ast ])
            | `Sigi (s1, s2) ->
                f s1.psig_loc (printed_ast add_comments s1.psig_loc Signature [ s2.ast ])
            | `Me (v1, v2) ->
                f v1.pmod_loc
                  (String.lstrip
                     (printed_ast add_comments v1.pmod_loc Module_expr v2.ast))
            | `Typ (t1, t2) ->
                f t1.ptyp_loc
                  (parens_if (Ast.parenze_typ t2)
                     (printed_ast add_comments t1.ptyp_loc Core_type t2.ast))
            | `Cf (v1, v2) ->
                f v1.pcf_loc (printed_ast add_comments v1.pcf_loc Class_field v2.ast)
            | `Cty (v1, v2) ->
                f v1.pcty_loc
                  (parens_if (Ast.parenze_cty v2)
                     (printed_ast add_comments v1.pcty_loc Class_type v2.ast))
            | `Rem loc -> f loc ""
            | `Add_stri (loc, (indent, s2)) ->
                f loc
                  (String.make indent ' '
                  ^ printed_ast add_comments loc Structure [ s2 ]
                  ^ "\n")
            | `Add_sigi (loc, (indent, s2)) ->
                f loc
                  (String.make indent ' '
                  ^ printed_ast add_comments loc Signature [ s2 ]
                  ^ "\n")))

(* problems:
   - record fields should be special cased like variants
 *)
