let limit = ref 1000
let debug_parsing = ref false
let debug_typing = ref false
let debug_knormal = ref false
let debug_alpha = ref false
let debug_closure = ref false
let debug_virtual = ref false
let debug_simm = ref false
let debug_regalloc = ref false

let rec iter n e = (* ��Ŭ�������򤯤꤫���� (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

let lexbuf outchan l = (* �Хåե��򥳥�ѥ��뤷�ƥ����ͥ�ؽ��Ϥ��� (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan
    (RegAlloc.f !debug_regalloc
       (Simm.f !debug_simm
          (Virtual.f !debug_virtual
             (Closure.f !debug_closure
                (iter !limit
                   (Alpha.f !debug_alpha
                      (KNormal.f !debug_knormal
                         (Typing.f !debug_typing
                            (Parser.exp !debug_parsing Lexer.token l)))))))))

let string s = lexbuf stdout (Lexing.from_string s) (* ʸ����򥳥�ѥ��뤷��ɸ����Ϥ�ɽ������ (caml2html: main_string) *)

let file f = (* �ե�����򥳥�ѥ��뤷�ƥե�����˽��Ϥ��� (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* �������饳��ѥ���μ¹Ԥ����Ϥ���� (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-debug-parsing", Arg.Bool(fun b -> debug_parsing := b), "if true prints the result of parsing");
     ("-debug-typing", Arg.Bool(fun b -> debug_typing := b), "if true prints the result of knormal");
     ("-debug-knormal", Arg.Bool(fun b -> debug_knormal := b), "if true prints the result of parsing");
     ("-debug-alpha", Arg.Bool(fun b -> debug_alpha := b), "if true prints the result of alpha");
     ("-debug-closure", Arg.Bool(fun b -> debug_closure := b), "if true prints the result of closure conversion");
     ("-debug-virtual", Arg.Bool(fun b -> debug_virtual := b), "if true prints the result of virtual");
     ("-debug-simm", Arg.Bool(fun b -> debug_simm := b), "if true prints the result of simm");
     ("-debug-regalloc", Arg.Bool(fun b -> debug_regalloc := b), "if true prints the result of register allocation");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] [-debug-parsing b1] [-debug-typing b2]...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
