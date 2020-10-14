(* 2���ڥ��ɤǤϤʤ�3���ڥ��ɤ�x86������֥��ɤ� *)

type id_or_imm = V of Id.t | C of int
type t = (* ̿����� (caml2html: sparcasm_t) *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* ��İ�Ĥ�̿����б����뼰 (caml2html: sparcasm_exp) *)
  | Nop
  | Set of int
  | SetL of Id.l
  | Mov of Id.t
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm * int
  | St of Id.t * Id.t * id_or_imm * int
  | FMovD of Id.t
  | FNegD of Id.t
  | FAddD of Id.t * Id.t
  | FSubD of Id.t * Id.t
  | FMulD of Id.t * Id.t
  | FDivD of Id.t * Id.t
  | LdDF of Id.t * id_or_imm * int
  | StDF of Id.t * Id.t * id_or_imm * int
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t (* �����оΤǤϤʤ��Τ�ɬ�� *)
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* �쥸�����ѿ����ͤ򥹥��å��ѿ�����¸ (caml2html: sparcasm_save) *)
  | Restore of Id.t (* �����å��ѿ������ͤ����� (caml2html: sparcasm_restore) *)
type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
(* �ץ���������� = ��ư���������ơ��֥� + �ȥåץ�٥�ؿ� + �ᥤ��μ� (caml2html: sparcasm_prog) *)
type prog = Prog of (Id.l * float) list * fundef list * t


(* id_or_imm を string に変換する *)
let stringify_id_or_imm (i: id_or_imm) : string =
  match i with
  | V id -> id
  | C j -> string_of_int j

(* Asm.t を string に変換する *)
let rec stringify (e: t) (level: int) : string =
  (Syntax.repeat "  " level) ^ (
    match e with
    | Ans ex -> "ANS\n" ^ (stringify_exp ex (level + 1))
    | Let (varexp, expex, expt) -> "LET " ^ (Syntax.stringify_vardef varexp) ^ "\n"
      ^ (stringify_exp expex (level + 1))
      ^ (stringify expt (level + 1))
  )
(* Asm.exp を string に変換する *)
and stringify_exp (e: exp) (level: int) : string =
  (Syntax.repeat "  " level) ^ (
    match e with
    | Nop -> "NOP\n"
    | Set i -> "SET " ^ (string_of_int i) ^ "\n"
    | SetL (L id) -> "SETL " ^ id ^ "\n"
    | Mov id -> "MOV " ^ id ^ "\n"
    | Neg id -> "NEG " ^ id ^ "\n"
    | Add (id, idm) -> "ADD " ^ id ^ " " ^ (stringify_id_or_imm idm) ^ "\n"
    | Sub (id, idm) -> "SUB " ^ id ^ " " ^ (stringify_id_or_imm idm) ^ "\n"
    | Ld (id, idm, i) -> "LD " ^ id ^ " " ^ (stringify_id_or_imm idm) ^ " " ^ (string_of_int i) ^ "\n"
    | St (id1, id2, idm, i) -> "SD " ^ id1 ^ " " ^ id2 ^ " " ^ (stringify_id_or_imm idm) ^ (string_of_int i) ^ "\n"
    | FMovD id -> "FMOVD " ^ id ^ "\n"
    | FNegD id -> "FNEGD " ^ id ^ "\n"
    | FAddD (id1, id2) -> "FADDD " ^ id1 ^ " " ^ id2 ^ "\n"
    | FSubD (id1, id2) -> "FSUBD " ^ id1 ^ " " ^ id2 ^ "\n"
    | FMulD (id1, id2) -> "FMULD " ^ id1 ^ " " ^ id2 ^ "\n"
    | FDivD (id1, id2) -> "FDIVD " ^ id1 ^ " " ^ id2 ^ "\n"
    | LdDF (id, idm, i) -> "LDDF " ^ id ^ " " ^ (stringify_id_or_imm idm) ^ " " ^ (string_of_int i) ^ "\n"
    | StDF (id1, id2, idm, i) -> "SDDF " ^ id1 ^ " " ^ id2 ^ " " ^ (stringify_id_or_imm idm) ^ (string_of_int i) ^ "\n"
    | Comment str -> "COMMENT " ^ str ^ "\n"
    | IfEq (id, idm, expt1, expt2) -> "IFEQ " ^ id ^ " " ^ (stringify_id_or_imm idm) ^ "\n"
      ^ (stringify expt1 (level + 1))
      ^ (stringify expt2 (level + 1))
    | IfLE (id, idm, expt1, expt2) -> "IFLE " ^ id ^ " " ^ (stringify_id_or_imm idm) ^ "\n"
      ^ (stringify expt1 (level + 1))
      ^ (stringify expt2 (level + 1))
    | IfGE (id, idm, expt1, expt2) -> "IFGE " ^ id ^ " " ^ (stringify_id_or_imm idm) ^ "\n"
      ^ (stringify expt1 (level + 1))
      ^ (stringify expt2 (level + 1))
    | IfFEq (id1, id2, expt1, expt2) -> "IFFEQ " ^ id1 ^ " " ^ id2 ^ "\n"
      ^ (stringify expt1 (level + 1))
      ^ (stringify expt2 (level + 1))
    | IfFLE (id1, id2, expt1, expt2) -> "IFFLE " ^ id1 ^ " " ^ id2 ^ "\n"
      ^ (stringify expt1 (level + 1))
      ^ (stringify expt2 (level + 1))
    | CallCls (id, idls1, idls2) -> "CALLCLS " ^ id ^ "\n"
      ^ (Syntax.repeat "  " (level + 1)) ^ (String.concat " " idls1) ^ "\n"
      ^ (Syntax.repeat "  " (level + 1)) ^ (String.concat " " idls2) ^ "\n"
    | CallDir (L id, idls1, idls2) -> "CALLDIR " ^ id ^ "\n"
      ^ (Syntax.repeat "  " (level + 1)) ^ (String.concat " " idls1) ^ "\n"
      ^ (Syntax.repeat "  " (level + 1)) ^ (String.concat " " idls2) ^ "\n"
    | Save (id1, id2) -> "SAVE " ^ id1 ^ " " ^ id2 ^ "\n"
    | Restore id -> "RESTORE " ^ id ^ "\n"
  )

(* Asm.fundef を string に変換する *)
let stringify_fundef (exp_f: fundef) : string = "FUNDEF " ^ (match exp_f.name with L id -> id) ^ "\n"
  ^ "  ARGS " ^ (String.concat " " exp_f.args)  ^ "\n"
  ^ "  FARGS " ^ (String.concat " " exp_f.fargs) ^ "\n"
  ^ (stringify exp_f.body 1)
  ^ "  RET " ^ (Type.stringify exp_f.ret) ^ "\n"

(* (Id.l * float) のタプルを string に変換する *)
let stringify_l_f (l_f: (Id.l * float)) : string =
  match l_f with
  | (L id, f) -> id ^ " " ^ (string_of_float f)

(* Asm.prog を string に変換する *)
let stringify_prog (exp_prog: prog) : string =
  match exp_prog with Prog (id_fl_ls, exp_f_ls, expt) ->
  (String.concat " " (List.map stringify_l_f id_fl_ls)) ^ "\n"
  ^ (String.concat "" (List.map stringify_fundef exp_f_ls))
  ^ (stringify expt 0)

let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)
let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = (* Array.init 16 (fun i -> Printf.sprintf "%%r%d" i) *)
  [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi" |]
let fregs = Array.init 8 (fun i -> Printf.sprintf "%%xmm%d" i)
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
(*
let reg_sw = regs.(Array.length regs - 1) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
*)
let reg_sp = "%ebp" (* stack pointer *)
let reg_hp = "min_caml_hp" (* heap pointer (caml2html: sparcasm_reghp) *)
(* let reg_ra = "%eax" (* return address *) *)
let is_reg x = (x.[0] = '%' || x = reg_hp)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V(x) -> [x] | _ -> []
let rec fv_exp = function
  | Nop | Set(_) | SetL(_) | Comment(_) | Restore(_) -> []
  | Mov(x) | Neg(x) | FMovD(x) | FNegD(x) | Save(x, _) -> [x]
  | Add(x, y') | Sub(x, y') | Ld(x, y', _) | LdDF(x, y', _) -> x :: fv_id_or_imm y'
  | St(x, y, z', _) | StDF(x, y, z', _) -> x :: y :: fv_id_or_imm z'
  | FAddD(x, y) | FSubD(x, y) | FMulD(x, y) | FDivD(x, y) -> [x; y]
  | IfEq(x, y', e1, e2) | IfLE(x, y', e1, e2) | IfGE(x, y', e1, e2) -> x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | IfFEq(x, y, e1, e2) | IfFLE(x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let align i = (if i mod 8 = 0 then i else i + 4)
