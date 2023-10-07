open Prelude

type typ =
  | TypeAlloc of typ list
  | TypeBool
  | TypeCaptures of (string * typ) list
  | TypeFunc of typ list * (int, (string * typ) list) Either.t * typ
  | TypeInt
  | TypeVar of int

type expr =
  | ExprAlloc of expr list
  | ExprBool of bool
  | ExprCall of expr * expr list
  | ExprDeref of expr * int
  | ExprFunc of func
  | ExprIdent of string
  | ExprInt of int
  | ExprUndef

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

type expr_type =
  | ExprTypeAlloc of expr_type list * typ
  | ExprTypeBool of bool
  | ExprTypeCall of expr_type * expr_type list * typ
  | ExprTypeDeref of expr_type * int * typ
  | ExprTypeFunc of func_type * typ
  | ExprTypeIdent of string * typ
  | ExprTypeInt of int
  | ExprTypeUndef of typ

and stmt_type =
  | StmtTypeLet of string * expr_type
  | StmtTypeSet of expr_type * expr_type
  | StmtTypeVoid of expr_type

and func_type = {
  args : (string * typ) list;
  captures : (string * typ) list;
  body : stmt_type list;
  return : expr_type;
}

type state = {
  mutable k : int;
  constraints : (int, typ) Hashtbl.t;
}

let global = {
  k = 0;
  constraints = Hashtbl.create 64;
}

let intrinsics =
  [
    ("+", TypeFunc ([TypeInt; TypeInt], Either.right [], TypeInt));
    ("print", TypeFunc ([TypeInt], Either.right [], TypeInt));
  ]
  |> List.to_seq
  |> Hashtbl.of_seq

let rec show_type =
  function
  | TypeAlloc types ->
    List.map show_type types
    |> String.concat ", "
    |> Printf.sprintf "[%s]"
  | TypeBool -> "bool"
  | TypeCaptures captures ->
    List.map
      (fun (s, t) -> Printf.sprintf "%s: %s" s (show_type t))
      captures
    |> String.concat ", "
    |> Printf.sprintf "|%s|"
  | TypeFunc (args, captures, return) ->
    let captures =
      match captures with
      | Right captures ->
        List.map
          (fun (s, t) -> Printf.sprintf "%s %s" s (show_type t))
          captures
        |> String.concat ", "
        |> Printf.sprintf "|%s|"
      | Left k -> Printf.sprintf "#%d" k in
    Printf.sprintf "\\(%s) %s -> %s"
      (String.concat ", " (List.map show_type args))
      captures
      (show_type return)
  | TypeInt -> "int"
  | TypeVar k -> Printf.sprintf "#%d" k

let indent n =
  String.make n ' '

let rec show_expr n =
  function
  | ExprAlloc exprs ->
    List.map (show_expr n) exprs
    |> String.concat ", "
    |> Printf.sprintf "[%s]"
  | ExprBool b -> string_of_bool b
  | ExprCall (func, args) ->
    List.map (show_expr n) args
    |> String.concat ", "
    |> Printf.sprintf "%s(%s)" (show_expr n func)
  | ExprDeref (expr, n) -> Printf.sprintf "%s[%d]" (show_expr n expr) n
  | ExprFunc func -> show_func n func
  | ExprIdent s -> s
  | ExprInt n -> string_of_int n
  | ExprUndef -> "_"

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

let rec show_expr_type n =
  function
  | ExprTypeAlloc (exprs, t) ->
    (
      List.map (show_expr_type n) exprs
      |> String.concat ", "
      |> Printf.sprintf "([%s] %s)"
    ) (show_type t)
  | ExprTypeBool b -> Printf.sprintf "(%b bool)" b
  | ExprTypeCall (func, args, t) ->
    (
      List.map (show_expr_type n) args
      |> String.concat ", "
      |> Printf.sprintf "(%s(%s) %s)" (show_expr_type n func)
    ) (show_type t)
  | ExprTypeDeref (expr, n, t) ->
    Printf.sprintf "(%s[%d] %s)" (show_expr_type n expr) n (show_type t)
  | ExprTypeFunc (func, t) ->
    Printf.sprintf "(%s %s)" (show_func_type n func) (show_type t)
  | ExprTypeIdent (s, t) -> Printf.sprintf "(%s %s)" s (show_type t)
  | ExprTypeInt n -> Printf.sprintf "(%d int)" n
  | ExprTypeUndef t -> Printf.sprintf "(_ %s)" (show_type t)

and show_stmt_type n =
  function
  | StmtTypeLet (ident, value) ->
    Printf.sprintf "%s := %s" ident (show_expr_type n value)
  | StmtTypeSet (target, value) ->
    Printf.sprintf "%s = %s" (show_expr_type n target) (show_expr_type n value)
  | StmtTypeVoid expr -> show_expr_type n expr

and show_func_type n (func : func_type) =
  Printf.sprintf "\\(%s) |%s| {\n%s%s}"
    (
      List.map
        (fun (s, t) -> Printf.sprintf "%s %s" s (show_type t))
        func.args
      |> String.concat ", "
    )
    (
      List.map
        (fun (s, t) -> Printf.sprintf "%s %s" s (show_type t))
        func.captures
      |> String.concat ", "
    )
    (
      let n = n + 4 in
      [show_expr_type n func.return]
      |> List.append
        (List.map (Printf.sprintf "%s;" |. show_stmt_type n) func.body)
      |> List.map (Printf.sprintf "%s%s\n" (indent n))
      |> String.concat ""
    )
    (indent n)

let rec find_captures_expr locals captures =
  function
  | ExprAlloc exprs ->
    ExprAlloc (List.map (find_captures_expr locals captures) exprs)
  | ExprCall (func, args) ->
    let func = find_captures_expr locals captures func in
    let args = List.map (find_captures_expr locals captures) args in
    ExprCall (func, args)
  | ExprDeref (expr, n) ->
    ExprDeref (find_captures_expr locals captures expr, n)
  | ExprFunc func ->
    (
      let func : func = find_captures_func func in
      (match func.captures with
       | Some cs ->
         List.iter
           (
             fun s ->
               if not (Hashtbl.mem locals s) then (
                 Hashtbl.replace captures s ()
               )
           )
           cs
       | _ -> assert false);
      ExprFunc func
    )
  | ExprIdent s as e when Hashtbl.mem locals s -> e
  | ExprIdent s as e ->
    (
      Hashtbl.replace captures s ();
      e
    )
  | ExprBool _ as e -> e
  | ExprInt _ as e -> e
  | ExprUndef as e -> e

and find_captures_stmt locals captures =
  function
  | StmtLet (ident, value) ->
    (
      let value = find_captures_expr locals captures value in
      Hashtbl.add locals ident ();
      StmtLet (ident, value)
    )
  | StmtSet (target, value) ->
    let target = find_captures_expr locals captures target in
    let value = find_captures_expr locals captures value in
    StmtSet (target, value)
  | StmtVoid expr -> StmtVoid (find_captures_expr locals captures expr)

and find_captures_func func =
  let locals =
    List.to_seq func.args
    |> Seq.map (fun s -> (s, ()))
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

let extract =
  function
  | ExprTypeAlloc (_, t) -> t
  | ExprTypeBool _ -> TypeBool
  | ExprTypeCall (_, _, t) -> t
  | ExprTypeDeref (_, _, t) -> t
  | ExprTypeFunc (_, t) -> t
  | ExprTypeIdent (_, t) -> t
  | ExprTypeInt _ -> TypeInt
  | ExprTypeUndef t -> t

let rec strip_expr =
  function
  | ExprTypeAlloc (exprs, _) -> ExprAlloc (List.map strip_expr exprs)
  | ExprTypeBool b -> ExprBool b
  | ExprTypeCall (func, args, _) ->
    ExprCall (strip_expr func, List.map strip_expr args)
  | ExprTypeDeref (expr, n, _) -> ExprDeref (strip_expr expr, n)
  | ExprTypeFunc (func, _) -> ExprFunc (strip_func func)
  | ExprTypeIdent (s, _) -> ExprIdent s
  | ExprTypeInt n -> ExprInt n
  | ExprTypeUndef _ -> ExprUndef

and strip_stmt =
  function
  | StmtTypeLet (ident, value) -> StmtLet (ident, strip_expr value)
  | StmtTypeSet (target, value) ->
    StmtSet (strip_expr target, strip_expr value)
  | StmtTypeVoid expr -> StmtVoid (strip_expr expr)

and strip_func (func : func_type) : func =
  {
    args = List.map fst func.args;
    captures = Some (List.map fst func.captures);
    body = List.map strip_stmt func.body;
    return = strip_expr func.return;
  }

let next_k () =
  let k = global.k in
  global.k <- global.k + 1;
  k

let var () =
  TypeVar (next_k ())

let captures_to_type : (int, (string * typ) list) Either.t -> typ =
  function
  | Either.Right captures -> TypeCaptures captures
  | Either.Left k -> TypeVar k

let rec bind =
  function
  | ((TypeCaptures _ as a), (TypeCaptures _ as b)) ->
    (
      Hashtbl.add global.constraints (next_k ()) a;
      Hashtbl.add global.constraints (next_k ()) b
    )
  | (TypeVar k, a) | (a, TypeVar k) ->
    (match Hashtbl.find_opt global.constraints k with
     | Some b ->
       (
         Hashtbl.remove global.constraints k;
         bind (a, b);
         Hashtbl.add global.constraints k b
       )
     | None -> Hashtbl.add global.constraints k a)
  | (
    TypeFunc (args0, captures0, return0),
    TypeFunc (args1, captures1, return1)
  ) ->
    (
      List.iter bind (List.combine args0 args1);
      bind (captures_to_type captures0, captures_to_type captures1);
      bind (return0, return1)
    )
  | (a, b) -> assert (a = b)

let rec find_type_expr locals : expr -> expr_type =
  function
  | ExprAlloc exprs ->
    let exprs = List.map (find_type_expr locals) exprs in
    ExprTypeAlloc (exprs, TypeAlloc (List.map extract exprs))
  | ExprBool b -> ExprTypeBool b
  | ExprCall (func, args) ->
    let func = find_type_expr locals func in
    let args = List.map (find_type_expr locals) args in
    (match extract func with
     | TypeFunc (arg_types, (Right capture_types), return_type) ->
       (
         assert (List.length args = List.length arg_types);
         List.iter
           (fun (s, t) -> bind (extract s, t))
           (List.combine args arg_types);
         List.iter
           (fun (s, t) -> assert (Hashtbl.find locals s = t))
           capture_types;
         ExprTypeCall (func, args, return_type)
       )
     | t ->
       (
         let return_type = var () in
         let captures = Either.left (next_k ()) in
         bind (t, TypeFunc (List.map extract args, captures, return_type));
         ExprTypeCall (func, args, return_type)
       ))
  | ExprDeref (expr, n) ->
    let expr = find_type_expr locals expr in
    (match extract expr with
     | TypeAlloc types -> ExprTypeDeref (expr, n, List.nth types n)
     | _ -> assert false)
  | ExprFunc func ->
    let func = find_type_func locals func in
    let args = List.map snd func.args in
    let captures = Either.right func.captures in
    ExprTypeFunc (func, TypeFunc (args, captures, extract func.return))
  | ExprIdent s  -> ExprTypeIdent (s, Hashtbl.find locals s)
  | ExprInt n -> ExprTypeInt n
  | ExprUndef -> ExprTypeUndef (var ())

and find_type_stmt locals : stmt -> stmt_type =
  function
  | StmtLet (ident, value) ->
    (
      if Hashtbl.mem locals ident then (
        assert false
      );
      let value = find_type_expr locals value in
      Hashtbl.add locals ident (extract value);
      StmtTypeLet (ident, value)
    )
  | StmtSet (target, value) ->
    (
      let target = find_type_expr locals target in
      let value = find_type_expr locals value in
      bind (extract target, extract value);
      StmtTypeSet (target, value)
    )
  | StmtVoid expr -> StmtTypeVoid (find_type_expr locals expr)

and find_type_func locals (func : func) : func_type =
  let args = List.map (fun s -> (s, var ())) func.args in
  let captures =
    match func.captures with
    | Some captures -> List.map (fun s -> (s, Hashtbl.find locals s)) captures
    | None -> assert false in
  List.iter (fun (k, v) -> Hashtbl.add locals k v) args;
  let body = List.map (find_type_stmt locals) func.body in
  let return = find_type_expr locals func.return in
  List.iter (Hashtbl.remove locals) func.args;
  { args; captures; body; return }

let rec resolve_type =
  let visited = Hashtbl.create 8 in
  let rec inner =
    function
    | TypeAlloc types -> TypeAlloc (List.map inner types)
    | TypeBool -> TypeBool
    | TypeCaptures captures ->
      TypeCaptures (List.map (fun (s, t) -> (s, inner t)) captures)
    | TypeFunc (args, captures, return) ->
      let args = List.map inner args in
      let captures =
        match inner (captures_to_type captures) with
        | TypeCaptures captures -> Either.Right captures
        | TypeVar k -> Either.Left k
        | _ -> assert false in
      TypeFunc (args, captures, inner return)
    | TypeInt -> TypeInt
    | TypeVar k ->
      (
        if Hashtbl.mem visited k then (
          TypeVar k
        ) else (
          Hashtbl.add visited k ();
          let t =
            match Hashtbl.find_opt global.constraints k with
            | Some t -> inner t
            | None -> TypeVar k in
          Hashtbl.replace global.constraints k t;
          Hashtbl.remove visited k;
          t
        )
      ) in
  inner

let resolve_constraints () =
  Hashtbl.to_seq_keys global.constraints
  |> Seq.iter
    (
      fun k ->
        let t = resolve_type (Hashtbl.find global.constraints k) in
        Hashtbl.replace global.constraints k t
    )

let rec resolve_expr =
  function
  | ExprTypeAlloc (exprs, t) ->
    ExprTypeAlloc (List.map resolve_expr exprs, resolve_type t)
  | ExprTypeBool b -> ExprTypeBool b
  | ExprTypeCall (func, args, t) ->
    let args = List.map resolve_expr args in
    ExprTypeCall (resolve_expr func, args, resolve_type t)
  | ExprTypeDeref (expr, n, t) ->
    ExprTypeDeref (resolve_expr expr, n, resolve_type t)
  | ExprTypeFunc (func, t) -> ExprTypeFunc (resolve_func func, resolve_type t)
  | ExprTypeIdent (s, t) -> ExprTypeIdent (s, resolve_type t)
  | ExprTypeInt _ as e -> e
  | ExprTypeUndef t -> ExprTypeUndef (resolve_type t)

and resolve_stmt =
  function
  | StmtTypeLet (ident, value) -> StmtTypeLet (ident, resolve_expr value)
  | StmtTypeSet (target, value) ->
    StmtTypeSet (resolve_expr target, resolve_expr value)
  | StmtTypeVoid expr -> StmtTypeVoid (resolve_expr expr)

and resolve_func func =
  let args = List.map (fun (s, t) -> (s, resolve_type t)) func.args in
  let captures = List.map (fun (s, t) -> (s, resolve_type t)) func.captures in
  let body = List.map resolve_stmt func.body in
  let return = resolve_expr func.return in
  { args; captures; body; return }

let rec rewrite_expr queue =
  function
  | ExprTypeAlloc (exprs, t) ->
    ExprTypeAlloc (List.map (rewrite_expr queue) exprs, t)
  | ExprTypeCall (func, args, t) ->
    let func = rewrite_expr queue func in
    let args = List.map (rewrite_expr queue) args in
    (match extract func with
     | TypeFunc (_, Right captures, _) ->
       if List.length captures <> 0 then (
         let ident =
           match func with
           | ExprTypeIdent _ -> func
           | _ ->
             (
               let ident = Printf.sprintf "__%d__" (next_k ()) in
               Queue.push (StmtTypeLet (ident, func)) queue;
               ExprTypeIdent (ident, var ())
             ) in
         let env = ExprTypeDeref (ident, 1, TypeCaptures captures) in
         ExprTypeCall (ExprTypeDeref (ident, 0, var ()), List.cons env args, t)
       ) else (
         ExprTypeCall (func, args, t)
       )
     | _ -> assert false)
  | ExprTypeDeref (expr, n, t) -> ExprTypeDeref (rewrite_expr queue expr, n, t)
  | ExprTypeFunc (func, t) ->
    (
      let func = rewrite_func func in
      if List.length func.captures = 0 then (
        ExprTypeFunc (func, t)
      ) else (
        let ident = Printf.sprintf "__%d__" (next_k ()) in
        let captures =
          List.map (fun (s, t) -> ExprTypeIdent (s, t)) func.captures in
        let args =
          [
            (ExprTypeFunc (func, var ()));
            (ExprTypeAlloc (captures, TypeCaptures func.captures));
          ] in
        let closure = ExprTypeAlloc (args, var ()) in
        Queue.push (StmtTypeLet (ident, closure)) queue;
        ExprTypeIdent (ident, t)
      )
    )
  | ExprTypeBool _ as e -> e
  | ExprTypeIdent _ as e -> e
  | ExprTypeInt _ as e -> e
  | ExprTypeUndef _ as e -> e

and rewrite_stmt queue =
  function
  | StmtTypeLet (ident, value) -> StmtTypeLet (ident, rewrite_expr queue value)
  | StmtTypeSet (target, value) ->
    StmtTypeSet (rewrite_expr queue target, rewrite_expr queue value)
  | StmtTypeVoid expr -> StmtTypeVoid (rewrite_expr queue expr)

and rewrite_func func =
  let queue = Queue.create () in
  if List.length func.captures <> 0 then (
    List.iteri
      (
        fun i (s, t) ->
          let deref = ExprTypeDeref (ExprTypeIdent ("env", var ()), i, t) in
          Queue.push (StmtTypeLet (s, deref)) queue
      )
      func.captures
  );
  List.iter (fun s -> Queue.push (rewrite_stmt queue s) queue) func.body;
  let return = rewrite_expr queue func.return in
  { func with body = List.of_seq (Queue.to_seq queue); return }

let () =
  let call func args =
    ExprCall (func, args) in
  let ident s =
    ExprIdent s in
  let int_0 = ExprInt 0 in
  let int_1 = ExprInt 1 in
  let funcs : func array = [|
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
  let program : func =
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

  print_endline (show_func 0 program);
  print_newline ();

  let program = find_captures_func program in

  print_endline (show_func 0 program);
  print_newline ();

  let program = find_type_func intrinsics program in

  resolve_constraints ();
  Hashtbl.iter
    (fun k v -> Printf.printf "%d: %s\n" k (show_type v))
    global.constraints;
  print_newline ();

  program
  |> resolve_func
  |> rewrite_func
  |> strip_func
  |> show_func 0
  |> print_endline
