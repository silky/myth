%{
open Lang

exception Internal_error of string

let rec ctor_of_int (n:int) : exp =
  if n = 0
  then ECtor("O", [])
  else ECtor("S", [ctor_of_int (n-1)])

let rec list_of_exps (l:exp list) : exp =
  match l with
  | [] -> ECtor("Nil", [])
  | e::es -> ECtor("Cons", [e; (list_of_exps es)])

let rec appify (e:exp) (es:exp list) : exp =
  match es with
  | [] -> e
  | e'::es -> appify (EApp (e, e')) es
%}

%token <string> LID   (* lowercase identifiers *)
%token <string> UID   (* uppercase identifiers *)
%token <int> INT      (* integer constants translated to O, S(O), S(S(O)), etc. *)

%token FUN        (* fun *)
%token MATCH      (* match *)
%token WITH       (* with *)
%token TYPE       (* type *)
%token OF         (* of *)
%token LET        (* let *)
%token IN         (* in *)
%token REC        (* rec *)

%token HOLE       (* ? *)
%token IMPLIES    (* |> *)
%token EQ         (* = *)
%token ARR        (* -> *)
%token FATARR     (* => *)
%token COMMA      (* , *)
%token COLON      (* : *)
%token SEMI       (* ; *)
%token STAR       (* * *)
%token PIPE       (* | *)
%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token LBRACE     (* { *)
%token RBRACE     (* } *)
%token LBRACKET   (* [ *)
%token RBRACKET   (* ] *)

%token EOF

%start <Lang.prog> prog

%%

prog:
  | ds=decls p=synth_problem EOF
    { (List.rev ds, p) }

(***** Declarations {{{ *****)

decls:  (* NOTE: reversed *)
  | (* empty *)
    { [] }
  | ds=decls d=datatype
    { d::ds }
  | ds=decls l=letbind
    { l::ds }

datatype:
  | TYPE d=LID EQ cs=ctors
    { DData (d, List.rev cs) }

letbind:
  | LET x=LID COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, false, [], t, e) }
  | LET x=LID args=arg_list COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, false, List.rev args, t, e) }
  | LET REC x=LID args=arg_list COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, true, List.rev args, t, e) }

ctors:  (* NOTE: reversed *)
  | (* empty *)
    { [] }
  | cs=ctors c=ctor
    { c::cs }

ctor:
  | PIPE c=UID OF ts=star_typ_list
    { (c, List.rev ts) }
  | PIPE c=UID
    { (c, []) }

star_typ_list:  (* NOTE: reversed *)
  | t=typ
    { [t] }
  | ts=star_typ_list STAR t=typ
    { t::ts }

synth_problem:
  | LET x=LID COLON t=typ IMPLIES LBRACE es=evidencelist RBRACE EQ HOLE
    { (x, t, es) }

(***** }}} *****)

(***** Types {{{ *****)

typ:
  | t=typ_arr
    { t }

typ_arr:
  | t1=typ_base ARR t2=typ_arr
    { TArr (t1, t2) }
  | t=typ_base  { t }

typ_base:
  | d=LID
    { TBase d }
  | LPAREN t=typ RPAREN
    { t }

(***** }}} *****)

(***** Expressions {{{ *****)

exp:
  | e=exp_app
    { e }

exp_app:
  | e=exp_base es=exp_app_list
    { appify e (List.rev es) }
  | e=exp_base
    { e }

exp_app_list:     (* NOTE: reversed *)
  | e=exp_base
    { [e] }
  | es=exp_app_list e=exp_base
    { e::es }

arg:
  | LPAREN x=LID COLON t=typ RPAREN
    { (x, t) }

arg_list:   (* NOTE: reversed *)
  | (* Empty *)
    { [] }
  | xs=arg_list x=arg
    { x :: xs }


exp_base:
  | x=LID
    { EVar x }
  | FUN x=arg ARR e=exp
    { EFun (x, e) }
  | LET f=LID xs=arg_list COLON t=typ EQ e1=exp IN e2=exp
    { ELet (f, false, List.rev xs, t, e1, e2) }
  | LET REC f=LID xs=arg_list COLON t=typ EQ e1=exp IN e2=exp
    { ELet (f, true, List.rev xs, t, e1, e2) }
  | c=INT
    { ctor_of_int c }
  | c=UID
    { ECtor (c, []) }
  | c=UID LPAREN es=exp_comma_list RPAREN
    { ECtor (c, es) }
  | MATCH e=exp WITH bs=branches
    { EMatch (e, List.rev bs) }
  | LBRACKET l=exp_semi_list RBRACKET
    { list_of_exps l }
  | LPAREN e=exp RPAREN
    { e }

exp_comma_list:
  | (* empty *)
    { [] }
  | e=exp
    { [e] }
  | e=exp COMMA es=exp_comma_list_one
    { e :: List.rev es }

exp_comma_list_one:    (* NOTE: reversed *)
  | e=exp
    { [e] }
  | es=exp_comma_list_one COMMA e=exp
    { e :: es }

exp_semi_list:
  | (* empty *)
    { [] }
  | e=exp
    { [e] }
  | e=exp SEMI es=exp_semi_list_one
    { e :: List.rev es }

exp_semi_list_one:    (* NOTE: reversed *)
  | e=exp
    { [e] }
  | es=exp_semi_list_one SEMI e=exp
    { e :: es }


branches:   (* NOTE: reversed *)
  | (* empty *)
    { [] }
  | bs=branches b=branch
    { b::bs }

branch:
  | PIPE p=pat ARR e=exp
    { (p, e) }

pat:
  | c=UID LPAREN xs=varlist RPAREN
    { PCtor (c, xs) }
  | c=UID
    { PCtor (c, []) }

varlist:
  | (* empty *)
    { [] }
  | x=LID
    { [x] }
  | x=LID COMMA xs=varlist_one
    { x :: List.rev xs }

varlist_one:    (* NOTE: reversed *)
  | x=LID
    { [x] }
  | xs=varlist_one COMMA x=LID
    { x::xs }

evidence:
  | v=ev_val
    { v }

ev_val:
  | v=ev_val_ios
    { v }
  | v=ev_val_base
    { v }

ev_val_ios:
  | v=ev_val_io
    { EPFun [v] }
  | v1=ev_val_io PIPE vs=ev_val_ios_one
    { EPFun (v1 :: List.rev vs) }

ev_val_ios_one:   (* NOTE: reversed *)
  | v=ev_val_io
    { [v] }
  | v=ev_val_io PIPE vs=ev_val_ios_one
    { v :: vs }

ev_val_io:
  | v1=ev_val_base FATARR v2=ev_val_io_one
    { (v1, v2) }

ev_val_io_one:
  | v=ev_val_base
    { v }
  | v1=ev_val_base FATARR v2=ev_val_io_one
    { EPFun [(v1, v2)] }

ev_val_base:
  | x=LID
    { EVar x }
  | FUN x=arg ARR e=exp
    { EFun (x, e) }
  | c=UID LPAREN vs=ev_val_list RPAREN
    { ECtor (c, vs) }
  | c=INT
    { ctor_of_int c }
  | c=UID
    { ECtor (c, []) }
  | LBRACKET l=ev_val_semi_list RBRACKET
    { list_of_exps l }
  | LPAREN v=ev_val RPAREN
    { v }

ev_val_semi_list:
  | (* empty *)
    { [] }
  | v=ev_val
    { [v] }
  | v=ev_val SEMI vs=ev_val_semi_list_one
    { v :: List.rev vs }

ev_val_semi_list_one:
  | v=ev_val
    { [v] }
  | vs=ev_val_semi_list_one SEMI v=ev_val
    { v::vs }


ev_val_list:
  | (* empty *)
    { [] }
  | v=ev_val
    { [v] }
  | v=ev_val COMMA vs=ev_val_list_one
    { v :: List.rev vs }

ev_val_list_one:
  | v=ev_val
    { [v] }
  | vs=ev_val_list_one COMMA v=ev_val
    { v::vs }

evidencelist:
  | (* empty *)
    { [] }
  | v=evidence
    { [v] }
  | v=evidence SEMI vs=evidencelist_one
    { v :: List.rev vs }

evidencelist_one:    (* NOTE: reversed *)
  | v=evidence
    { [v] }
  | vs=evidencelist_one SEMI v=evidence
    { v::vs }

(***** }}} *****)
