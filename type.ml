type t = (* MinCaml�η���ɽ������ǡ����� (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* ���������ѿ����� *)

let rec stringify (ty : t) : string =
  match ty with
  | Unit -> "UNIT"
  | Bool -> "BOOL"
  | Int -> "INT"
  | Float -> "FLOAT"
  | Fun (tyls, tyrt) ->
    (match tyls with
    | [] -> stringify tyrt
    | _ :: _ -> (String.concat " -> " (List.map stringify tyls)) ^ " -> " ^ (stringify tyrt)
    )
  | Tuple tyls -> "(" ^ (String.concat " * " (List.map stringify tyls)) ^ ")"
  | Array ty'-> "ARRAY " ^ (stringify ty')
  | Var varoptref ->
    match !varoptref with
    | Some ty -> stringify ty
    | None -> "?T"