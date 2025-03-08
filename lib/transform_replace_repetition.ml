open Base
open Common
open! Transform_common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open! Fmast

module Tree : sig
  (** A tree represents an fmast expression or pattern, but where each tree element is a
      shallow expression and an explicit list of children.

      Say [1 + 2] would be:
      { self = `Exp "_ + _"
      ; children = [ { self = `Exp "1"
                     ; children = []
                     }
                   ; { self = `Exp "2"
                     ; children = []
                     }
                   ]
      }

      The point of this tree structure is that its type is vastly simpler than the regular
      AST types, and so it's possible to implement tree-like functionality without writing
      thousands of lines of code of boilerplate.

      The concrete tree functionality we use it for is finding commonalities between
      trees, for which we need the ability to do some kind of iter2, as well as zippers,
      which would be highly painful to write and maintain on the actual AST types. *)

  type node =
    [ `Exp of expression
    | `Pat of pattern
    ]
  [@@deriving sexp_of]

  val equal_node : node -> node -> bool

  type t =
    { self : node
    ; children : t list
    }
  [@@deriving sexp_of]

  type zipper_node =
    { parent : zipper
    ; self : node
    ; left_rev_children : t list
    ; right_children : t list
    }

  and zipper = zipper_node option [@@deriving sexp_of]

  val depth : zipper -> int
  val fill : zipper_node -> t -> zipper * t
  val top : zipper -> t -> t
  val create : node -> t

  (* val to_node : t -> node *)
  val to_exp : t -> expression

  (* val to_pat : t -> pattern *)
  val find : t -> (node -> bool) -> (zipper * t) option
end = struct
  type node =
    [ `Exp of expression
    | `Pat of pattern
    ]
  [@@deriving sexp_of]

  let equal_node =
    let self = { Ast_mapper.default_mapper with location = (fun _ _ -> Location.none) } in
    fun e1 e2 ->
      match (e1, e2) with
      | `Exp e1, `Exp e2 -> Poly.( = ) (self.expr self e1) (self.expr self e2)
      | `Pat e1, `Pat e2 -> Poly.( = ) (self.pat self e1) (self.pat self e2)
      | (`Exp _ | `Pat _), _ -> false

  type t =
    { self : node
    ; children : t list
    }

  let rec sexp_of_t { self; children } = [%sexp (self : node), (children : t list)]

  type zipper_node =
    { parent : zipper
    ; self : node
    ; left_rev_children : t list
    ; right_children : t list
    }

  and zipper = zipper_node option [@@deriving sexp_of]

  let rec depth acc = function
    | None -> acc
    | Some { parent; _ } -> depth (acc + 1) parent

  let depth tree = depth 0 tree

  let fill { parent; self; left_rev_children; right_children } tree =
    ( parent
    , { self; children = List.rev_append left_rev_children (tree :: right_children) } )

  let rec top zipper tree =
    match zipper with
    | None -> tree
    | Some { parent; self; left_rev_children; right_children } ->
        top parent
          { self; children = List.rev_append left_rev_children (tree :: right_children) }

  let rec create node =
    let dummy_expr = Ast_helper.Exp.hole () in
    let dummy_pat = Ast_helper.Pat.any () in
    let super = Ast_mapper.default_mapper in
    let children = ref [] in
    let self =
      { super with
        expr =
          (fun _ v ->
            children := `Exp v :: !children;
            dummy_expr)
      ; pat =
          (fun _ v ->
            children := `Pat v :: !children;
            dummy_pat)
      }
    in
    let shallow_expr =
      match node with
      | `Exp v -> `Exp (super.expr self v)
      | `Pat v -> `Pat (super.pat self v)
    in
    { self = shallow_expr; children = List.map (List.rev !children) ~f:create }

  let rec to_node t =
    let super = Ast_mapper.default_mapper in
    let children = ref t.children in
    let pop_exn () =
      match !children with
      | [] -> raise_s [%sexp "no more children??", (t : t)]
      | res :: rest ->
          children := rest;
          res
    in
    let self =
      { super with
        expr = (fun _ _ -> to_exp (pop_exn ()))
      ; pat = (fun _ _ -> to_pat (pop_exn ()))
      }
    in
    if List.is_empty t.children
    then t.self (* so we can stick complete expression *)
    else
      match t.self with
      | `Exp e -> `Exp (super.expr self e)
      | `Pat e -> `Pat (super.pat self e)

  and to_exp t = match to_node t with `Exp e -> e | _ -> assert false
  and to_pat t = match to_node t with `Pat e -> e | _ -> assert false

  let find tree f =
    With_return.with_return (fun r ->
        let rec find zipper ({ self; children } as t) =
          if f self
          then r.return (Some (zipper, t))
          else
            find_next_child
              { parent = zipper; self; left_rev_children = []; right_children = children }
        and find_next_child { parent; self; left_rev_children; right_children } =
          match right_children with
          | [] -> ()
          | child :: rest ->
              find (Some { parent; self; left_rev_children; right_children = rest }) child;
              find_next_child
                { parent
                ; self
                ; left_rev_children = child :: left_rev_children
                ; right_children = rest
                }
        in
        find None tree;
        None)
end

let x_var = function
  | `Expr
      ({ pexp_desc =
           Pexp_extension
             ( { txt = "X"; _ }
             , PStr
                 [ { pstr_desc =
                       Pstr_eval
                         ({ pexp_desc = Pexp_ident { txt = Lident motif; _ }; _ }, _)
                   ; _
                   }
                 ] )
       ; _
       } :
        expression) ->
      Some motif
  | `Pat
      ({ ppat_desc =
           Ppat_extension
             ( { txt = "X"; _ }
             , PStr
                 [ { pstr_desc =
                       Pstr_eval
                         ({ pexp_desc = Pexp_ident { txt = Lident motif; _ }; _ }, _)
                   ; _
                   }
                 ] )
       ; _
       } :
        pattern) ->
      Some motif
  | `Expr _ | `Pat _ -> None

let infer motif =
  (* To try to find repeated patterns, we first break up the input expression into
     sub-expressions, each with its own repetition. The criteria is that we find the
     small subexpressions that contain a complete set of variables (meaning all of
     a1, a2, a3, for all names "a").

     Then on each section, we find a repetition of the form [wrap (f (f base a2) a3)]
     by taking the tree centered at the first occurrence of a2, calling a2 X, taking
     the tree centered at a3, calling a3 X, and then taking the intersection between
     these two trees. Any node in the intersection is the repeating part, i.e. f. Any
     disagreement while we walk up becomes [wrap]. Any disagreement while we walk own
     becomes part of [base]. *)
  let debug = false in
  let display = false in
  if display then print_s [%sexp ~~(motif : expression)];
  let tree = Tree.create (`Exp motif) in
  if debug then print_s [%sexp ~~(tree : Tree.t)];
  let is_var s : Tree.node -> _ = function
    | `Exp { pexp_desc = Pexp_ident { txt = Lident s'; _ }; _ }
    | `Pat { ppat_desc = Ppat_var { txt = s'; _ }; _ } ->
        if String.is_prefix s' ~prefix:"__" then String.chop_suffix s' ~suffix:s else None
    | _ -> None
  in
  let is_v1 = is_var "1" in
  let is_v2 = is_var "2" in
  let is_v3 = is_var "3" in
  let rec split_up_independent_repeats (tree : Tree.t) f =
    let seens, children =
      List.map tree.children ~f:(split_up_independent_repeats __ f) |> List.unzip
    in
    let tree = { tree with children } in
    let seen =
      List.fold_left
        ~init:
          (match is_v1 tree.self with
          | Some s -> Map.singleton (module String) s 1
          | None -> (
              match is_v2 tree.self with
              | Some s -> Map.singleton (module String) s 2
              | None -> (
                  match is_v3 tree.self with
                  | Some s -> Map.singleton (module String) s 4
                  | None -> Map.empty (module String))))
        seens
        ~f:(Map.merge_skewed ~combine:(fun ~key:_ b1 b2 -> b1 lor b2))
    in
    if (not (Map.is_empty seen)) && Map.for_all seen ~f:(__ = 7)
    then (Map.empty (module String), f tree)
    else (seen, tree)
  in
  let matching_var2 self1 self2 =
    match (is_v2 self1, is_v3 self2) with Some s2, Some s3 -> s2 =: s3 | _ -> false
  in
  let matching_var1 self1 self2 =
    match (is_v1 self1, is_v2 self2) with Some s1, Some s2 -> s1 =: s2 | _ -> false
  in
  let _, result =
    split_up_independent_repeats tree (fun tree ->
        if debug then print_s [%sexp `found_chunk (tree : Tree.t)];
        let z1, a1 =
          Option.value_exn (Tree.find tree (fun t -> Option.is_some (is_v1 t)))
        in
        let z2, a2 =
          Option.value_exn (Tree.find tree (fun t -> Option.is_some (is_v2 t)))
        in
        let z3, a3 =
          Option.value_exn (Tree.find tree (fun t -> Option.is_some (is_v3 t)))
        in
        let cur_elt name =
          Ast_helper.Exp.ext_exp "X" (Ast_helper.Exp.ident' (Lident name))
        in
        let acc = Ast_helper.Exp.ext_exp "acc" (Ast_helper.Exp.unit ()) in
        let rec intersect_down (tree1 : Tree.t) (tree2 : Tree.t) =
          if
            Tree.equal_node tree1.self tree2.self
            || matching_var2 tree1.self tree2.self
            || matching_var1 tree1.self tree2.self
          then
            let has_acc = ref false in
            let has_none = ref false in
            let children =
              List.map2_exn tree1.children tree2.children ~f:(fun c1 c2 : Tree.t ->
                  match intersect_down c1 c2 with
                  | Some (c_has_acc, c_tree) ->
                      has_acc := !has_acc || c_has_acc;
                      c_tree
                  | None ->
                      has_none := true;
                      { self = `Exp (Ast_helper.Exp.hole ()); children = [] })
            in
            if !has_none
            then
              if !has_acc
              then Some (true, { self = `Exp acc; children = [ tree1 ] })
              else None
            else
              let this : Tree.t =
                { self =
                    (if Tree.equal_node tree1.self tree2.self
                     then tree1.self
                     else
                       match is_v2 tree1.self with
                       | Some var -> `Exp (cur_elt var)
                       | None ->
                           has_acc := true;
                           `Exp acc)
                ; children
                }
              in
              Some (!has_acc, this)
          else None
        in
        let rec join_up (r : _ With_return.return) (z1 : Tree.zipper) (z2 : Tree.zipper)
            (tree : Tree.t) =
          match (z1, z2) with
          | Some znode1, Some znode2
            when Tree.equal_node znode1.self znode2.self
                 || matching_var2 znode1.self znode2.self
                 || matching_var1 znode1.self znode2.self ->
              let this : Tree.t =
                { self =
                    (if Tree.equal_node znode1.self znode2.self
                     then znode1.self
                     else
                       match is_v2 znode1.self with
                       | Some var -> `Exp (cur_elt var)
                       | None ->
                           `Exp (Ast_helper.Exp.ext_exp "acc" (Ast_helper.Exp.unit ())))
                ; children =
                    (let f t1 t2 =
                       match intersect_down t1 t2 with
                       | None -> r.return None
                       | Some (_, v) ->
                           (* should not infer a motif if we have multiple has_acc *)
                           v
                     in
                     List.rev_map2_exn znode1.left_rev_children znode2.left_rev_children
                       ~f
                     @ tree
                       :: List.map2_exn znode1.right_children znode2.right_children ~f)
                }
              in
              join_up r znode1.parent znode2.parent this
          | _ ->
              Tree.top z2
                { self = `Exp (Ast_helper.Exp.ext_exp "repeat" (Ast_helper.Exp.unit ()))
                ; children = [ tree ]
                }
        in
        let tree =
          if Tree.depth z1 = Tree.depth z2 && Tree.depth z2 = Tree.depth z3
          then
            (* All the same depth means that this should be a flat repetition like (foo
               a1, foo a2, foo a3) (not that we're currently able to detect this
               pattern). *)
            let matching_names self1 self2 self3 =
              match is_v1 self1 with
              | None -> None
              | Some _ as opt ->
                  if
                    List.for_all
                      [ is_v2 self2; is_v3 self3 ]
                      ~f:([%equal: string option] opt)
                  then opt
                  else None
            in
            match (z1, z2, z3) with
            | Some p1, Some p2, Some p3 ->
                assert (Poly.( = ) (Tree.fill p1 a1) (Tree.fill p2 a2));
                assert (Poly.( = ) (Tree.fill p1 a1) (Tree.fill p3 a3));
                let p1f, a1f = Tree.fill p1 a1 in
                let expr =
                  match Tree.to_exp a1f with
                  | { pexp_desc = Pexp_function (params, typ, body); _ } as pat -> (
                      match params with
                      | [ { pparam_desc = Pparam_val (Nolabel, None, p1); _ }
                        ; { pparam_desc = Pparam_val (Nolabel, None, p2); _ }
                        ; { pparam_desc = Pparam_val (Nolabel, None, p3); _ }
                        ]
                        when Option.is_some (matching_names (`Pat p1) (`Pat p2) (`Pat p3))
                        ->
                          let var = Option.value_exn (is_v1 (`Pat p1)) in
                          { pat with
                            pexp_desc =
                              Pexp_function
                                ( [ { pparam_desc =
                                        Pparam_val
                                          ( Nolabel
                                          , None
                                          , Ast_helper.Pat.var
                                              { loc = Location.none; txt = var } )
                                    ; pparam_loc = Location.none
                                    }
                                  ]
                                , typ
                                , body )
                          }
                      | _ -> failwith "unhandled kind of flat repetition")
                  | { pexp_desc = Pexp_letop bindings; _ } as pat -> (
                      match bindings.let_ :: bindings.ands with
                      | [ { pbop_pat = p1; pbop_exp = e1; _ }
                        ; { pbop_pat = p2; pbop_exp = e2; _ }
                        ; { pbop_pat = p3; pbop_exp = e3; _ }
                        ] ->
                          let pats_change =
                            matching_names (`Pat p1) (`Pat p2) (`Pat p3)
                          in
                          let exps_change =
                            matching_names (`Exp e1) (`Exp e2) (`Exp e3)
                          in
                          { pat with
                            pexp_desc =
                              Pexp_letop
                                { bindings with
                                  ands = []
                                ; let_ =
                                    { bindings.let_ with
                                      pbop_pat =
                                        (match pats_change with
                                        | None -> p1
                                        | Some var ->
                                            Ast_helper.Pat.ext_exp "X"
                                              (Ast_helper.Exp.ident' (Lident var)))
                                    ; pbop_exp =
                                        (match exps_change with
                                        | None -> e1
                                        | Some var -> cur_elt var)
                                    }
                                }
                          }
                      | _ -> failwith "unhandled kind of flat repetition")
                  | _ -> failwith "unhandled kind of flat repetition"
                in
                let expr =
                  Ast_helper.Exp.extension
                    ( { txt = "repeat2"; loc = Location.none }
                    , PStr [ Ast_helper.Str.eval expr ] )
                in
                Tree.top p1f { self = `Exp expr; children = [] }
            | _ -> assert false
          else
            let tree =
              if matching_var2 a2.self a3.self && matching_var1 a1.self a2.self
              then
                let var = Option.value_exn (is_v2 a2.self) in
                With_return.with_return (fun r ->
                    Some (join_up r z2 z3 { self = `Exp (cur_elt var); children = [] }))
                |> Option.value ~default:tree
              else tree
            in
            if debug then print_s [%sexp ~~(tree : Tree.t)];
            tree
        in
        if debug then print_s [%sexp `end_of_chunk, ~~(Tree.to_exp tree : expression)];
        tree)
  in
  let expr = Tree.to_exp result in
  if display then print_s [%sexp `result, ~~(expr : expression)];
  expr

let repeat2_template (e : P.expression) (self : Ast_mapper.mapper) ~count:n ~env_pat
    ~env_exp =
  match e with
  | { pexp_desc =
        Pexp_letop { ands = []; let_ = { pbop_pat; pbop_exp; _ } as let_; body; loc_in }
    ; _
    } ->
      let pat =
        match x_var (`Pat pbop_pat) with
        | None -> Fn.const (self.pat self pbop_pat)
        | Some var -> fun i -> env_pat (var ^ Int.to_string (i + 1))
      in
      let exp =
        match x_var (`Expr pbop_exp) with
        | None -> Fn.const (self.expr self pbop_exp)
        | Some var -> fun i -> env_exp (var ^ Int.to_string (i + 1))
      in
      assert (n >= 1);
      let let_ands =
        List.init n ~f:(fun i ->
            { let_ with
              pbop_op =
                (if i = 0
                 then let_.pbop_op
                 else
                   { let_.pbop_op with
                     txt = "and" ^ String.drop_prefix let_.pbop_op.txt 3
                   })
            ; pbop_pat = pat i
            ; pbop_exp = exp i
            })
      in
      let let_ = List.hd_exn let_ands in
      let ands = List.tl_exn let_ands in
      { e with pexp_desc = Pexp_letop { let_; ands; loc_in; body = self.expr self body } }
  | { pexp_desc =
        Pexp_function
          ( [ { pparam_desc =
                  Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = var; _ }; _ })
              ; _
              }
            ]
          , None
          , Pfunction_body body )
    ; _
    } ->
      let params =
        List.init n ~f:(fun i ->
            { P.pparam_desc =
                P.Pparam_val (Nolabel, None, env_pat (var ^ Int.to_string (i + 1)))
            ; pparam_loc = Location.none
            })
      in
      { e with
        pexp_desc = Pexp_function (params, None, Pfunction_body (self.expr self body))
      }
  | _ -> failwith "unimplemented"
