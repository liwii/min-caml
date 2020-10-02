type t = (* MinCaml�ι�ʸ��ɽ������ǡ����� (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec repeat (s: string) (n: int): string =
  if n <= 0 then ""
  else s ^ (repeat s (n - 1))

let stringify_vardef (vardef: (Id.t * Type.t)): string =
  (fst vardef) ^ " : " ^ (Type.stringify (snd vardef))

let rec stringify (exp : t) (level: int): string =
  (repeat "  " level) ^
  (match exp with
  | Unit -> "UNIT" ^ "\n"
  | Bool true -> "BOOL t" ^ "\n"
  | Bool false -> "BOOL f" ^ "\n"
  | Int n -> "INT " ^ (string_of_int n) ^ "\n"
  | Float f -> "FLOAT " ^ (string_of_float f) ^ "\n"
  | Not exp1 -> "NOT\n" ^ (stringify exp1 (level + 1))
  | Neg exp1 -> "NEG\n" ^ (stringify exp1 (level + 1))
  | Add (exp1, exp2) -> "ADD\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | Sub (exp1, exp2) -> "SUB\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | FNeg exp1 -> "FNEG\n" ^ (stringify exp1 (level + 1))
  | FAdd (exp1, exp2) -> "FADD\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | FSub (exp1, exp2) -> "FSUB\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | FMul (exp1, exp2) -> "FMUL\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | FDiv (exp1, exp2) -> "FDIV\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | Eq (exp1, exp2) -> "EQ\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | LE (exp1, exp2) -> "LE\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1))
  | If (exp1, exp2, exp3) -> "IF\n" ^ (stringify exp1 (level + 1)) ^ (stringify exp2 (level + 1)) ^ (stringify exp3 (level + 1))
  | Let (expvar, exp2, exp3) -> "LET\n" ^ (repeat "  " (level + 1)) ^ (stringify_vardef expvar)
      ^ "\n" ^ (stringify exp2 (level + 1)) ^ (stringify exp3 (level + 1))
  | Var expId -> "VAR " ^ expId ^ "\n"
  | LetRec (expFunDef, exp1) -> "FUNDEF" ^ (repeat "  " (level + 1)) ^ (stringify_vardef expFunDef.name) ^ "\n"
    ^ (repeat "  " (level + 1)) ^ (String.concat ", " (List.map stringify_vardef expFunDef.args)) ^ "\n"
    ^ (stringify expFunDef.body (level + 1))
    ^ (stringify exp1 (level + 1))
  | App (expf, expls) -> "APP\n" ^ (stringify expf (level + 1)) ^ (String.concat "" (List.map (fun e -> stringify e (level + 1)) expls))
  | Tuple expls -> "TUPLE\n" ^ (String.concat "" (List.map (fun e -> stringify e (level + 1)) expls))
  | LetTuple (expvarls, expdef, expbody) -> "TUPLEDEF" ^ (repeat "  " (level + 1)) ^ (String.concat ", " (List.map stringify_vardef expvarls)) ^ "\n"
    ^ (stringify expdef (level + 1))
    ^ (stringify expbody (level + 1))
  | Array (explen, expval) -> "ARRAY\n" ^ (repeat "  " (level + 1)) ^ (stringify explen (level + 1)) ^ (stringify expval (level + 1))
  | Get (exparr, expidx) -> "GET\n" ^ (stringify exparr (level + 1)) ^ (stringify expidx (level + 1))
  | Put (exparr, expidx, expval) ->  "PUT\n" ^ (stringify exparr (level + 1)) ^ (stringify expidx (level + 1)) ^ (stringify expval (level + 1))
  )