open Prelude

type state = {
  mutable k : int;
}

let global = {
  k = 0;
}

let intrinsics =
  [
    ("+", ());
    ("print", ());
  ]
  |> List.to_seq
  |> Hashtbl.of_seq

type expr =
  | ExprAlloc of expr list
  | ExprCall of expr * expr list
  | ExprDeref of expr * int
  | ExprFunc of func
  | ExprIdent of string
  | ExprInt of int

and stmt =
  | StmtLet of string * expr
  | StmtSet of expr * expr
  | StmtVoid of expr

and func = {
  args : string list;
  captures : string list option;
  body : stmt list;
  return : expr;
}

let indent n =
  String.make n ' '

let rec show_expr n =
  function
  | ExprAlloc exprs ->
    List.map (show_expr n) exprs
    |> String.concat ", "
    |> Printf.sprintf "[%s]"
  | ExprCall (func, args) ->
    List.map (show_expr n) args
    |> String.concat ", "
    |> Printf.sprintf "%s(%s)" (show_expr n func)
  | ExprDeref (expr, n) -> Printf.sprintf "%s[%d]" (show_expr n expr) n
  | ExprFunc func -> show_func n func
  | ExprIdent s -> s
  | ExprInt n -> string_of_int n

and show_stmt n =
  function
  | StmtLet (ident, value) ->
    Printf.sprintf "%s := %s" ident (show_expr n value)
  | StmtSet (target, value) ->
    Printf.sprintf "%s = %s" (show_expr n target) (show_expr n value)
  | StmtVoid expr -> show_expr n expr

and show_func n func =
  Printf.sprintf "\\(%s)%s {\n%s%s}"
    (String.concat ", " func.args)
    (
      Option.value
        (
          Option.map
            (Printf.sprintf " |%s|" |. String.concat ", ")
            func.captures
        )
        ""
    )
    (
      let n = n + 4 in
      [show_expr n func.return]
      |> List.append
        (List.map (Printf.sprintf "%s;" |. show_stmt n) func.body)
      |> List.map (Printf.sprintf "%s%s\n" (indent n))
      |> String.concat ""
    )
    (indent n)

let rec find_captures_expr locals captures =
  function
  | ExprAlloc exprs ->
    ExprAlloc (List.map (find_captures_expr locals captures) exprs)
  | ExprCall (func, args) ->
    ExprCall
      (
        find_captures_expr locals captures func,
        List.map (find_captures_expr locals captures) args
      )
  | ExprDeref (expr, n) ->
    ExprDeref (find_captures_expr locals captures expr, n)
  | ExprFunc func ->
    (
      let func = find_captures_func func in
      (match func.captures with
       | Some cs ->
         List.iter
           (
             fun c ->
               if not (Hashtbl.mem locals c) then
                 Hashtbl.replace captures c ()
           )
           cs
       | _ -> assert false);
      ExprFunc func
    )
  | (ExprIdent s) as expr when Hashtbl.mem locals s -> expr
  | (ExprIdent s) as expr ->
    (
      Hashtbl.replace captures s ();
      expr
    )
  | (ExprInt n) as expr -> expr

and find_captures_stmt locals captures =
  function
  | StmtLet (ident, value) ->
    (
      let value = find_captures_expr locals captures value in
      Hashtbl.add locals ident ();
      StmtLet (ident, value)
    )
  | StmtSet (target, value) ->
    StmtSet
      (
        find_captures_expr locals captures target,
        find_captures_expr locals captures value
      )
  | StmtVoid expr -> StmtVoid (find_captures_expr locals captures expr)

and find_captures_func func =
  let locals =
    List.to_seq func.args
    |> Seq.map (fun a -> (a, ()))
    |> Hashtbl.of_seq in
  Hashtbl.iter (fun k _ -> Hashtbl.add locals k ()) intrinsics;
  let captures = Hashtbl.create 16 in
  let body = List.map (find_captures_stmt locals captures) func.body in
  {
    args = func.args;
    captures =
      Hashtbl.to_seq captures
      |> Seq.map fst
      |> List.of_seq
      |> Option.some;
    body = body;
    return = find_captures_expr locals captures func.return;
  }

let () =
  let call func args =
    ExprCall (func, args) in
  let ident s =
    ExprIdent s in
  let int_0 = ExprInt 0 in
  let int_1 = ExprInt 1 in
  let funcs = [|
    {
      args = ["n"];
      captures = None;
      body = [StmtLet ("x", ident "n")];
      return =
        ExprFunc {
          args = ["m"];
          captures = None;
          body = [
            StmtSet (ident "x", call (ident "+") [ident "x"; ident "k"]);
            StmtSet (ident "x", call (ident "+") [ident "x"; ident "m"]);
          ];
          return = ident "x";
        };
    };
    {
      args = [];
      captures = None;
      body = [];
      return = call (ident "print") [call (ident "c") [int_1]];
    };
    {
      args = ["c"];
      captures = None;
      body = [];
      return = call (ident "print") [call (ident "c") [int_1]];
    };
  |] in
  let program =
    {
      args = [];
      captures = None;
      body = [
        StmtLet ("k", int_0);
        StmtLet ("counter", ExprFunc funcs.(0));
        StmtVoid (call (call (ident "counter") [int_0]) [int_1]);
        StmtLet ("c", call (ident "counter") [int_0]);
        StmtVoid (call (ExprFunc funcs.(1)) []);
        StmtVoid (call (ident "c") [int_1]);
        StmtVoid (call (ExprFunc funcs.(2)) [ident "c"]);
      ];
      return = int_0;
    } in
  program
  |> find_captures_func
  |> show_func 0
  |> print_endline
