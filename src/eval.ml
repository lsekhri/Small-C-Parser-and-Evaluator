open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec lookup env x=
  match env with 
  []-> raise (DeclareError "the environment does not have a binding")
  |(y,v)::env'-> if x=y then v
  else lookup env' x

let rec eval_expr env t =
match t with 
| Int i -> Int_Val i

| Bool b -> Bool_Val b

| ID d -> lookup env d

| Add (al1,al2) -> (match eval_expr env al1 with
|Int_Val ix -> (match eval_expr env al2 with
|Int_Val iy -> Int_Val(ix+iy)
|_ -> raise (TypeError "Type is incorrect"))
|_ -> raise (TypeError "Type is incorrect"))

|Sub(sl1,sl2) ->(match (eval_expr env sl1,eval_expr env sl2) with
|(Int_Val ix, Int_Val iv) -> Int_Val (ix-iv)
|_ -> raise (TypeError "Type is incorrect"))

|Mult(ml1,ml2) ->(match (eval_expr env ml1,eval_expr env ml2) with
|(Int_Val ix, Int_Val iv) -> Int_Val (ix*iv)
|_ -> raise (TypeError "Type is incorrect"))


|Div(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Int_Val ix,Int_Val iv) -> if iv=0 then raise (DivByZeroError) else Int_Val(ix/iv)
|_ -> raise (TypeError "Type is incorrect"))

|Pow(pl1,pl2) -> let n1=eval_expr env pl1 in
let n2=eval_expr env pl2 in
(match (n1,n2) with 
|(Int_Val ix,Int_Val iv) -> Int_Val(int_of_float((float_of_int ix)**(float_of_int iv)))
|_ -> raise (TypeError "Type is incorrect"))

|Or(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Bool_Val ix,Bool_Val iv) -> Bool_Val (ix||iv)
|_ -> raise (TypeError "Type is incorrect"))

|And(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Bool_Val ix,Bool_Val iv) -> Bool_Val (ix&&iv)
|_ -> raise (TypeError "Type is incorrect"))

|Not(nl1) -> let n1=eval_expr env nl1 in
(match n1 with 
| Bool_Val ic -> Bool_Val (not ic)
|_ -> raise (TypeError "Type is incorrect"))

|Less(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Int_Val ix,Int_Val iv) -> Bool_Val (ix<iv)
|_ -> raise (TypeError "Type is incorrect"))

|Greater(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Int_Val ix,Int_Val iv) -> Bool_Val (ix>iv)
|_ -> raise (TypeError "Type is incorrect"))

|LessEqual(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Int_Val ix,Int_Val iv) -> Bool_Val (ix<=iv)
|_ -> raise (TypeError "Type is incorrect"))

|GreaterEqual(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Int_Val ix,Int_Val iv) -> Bool_Val (ix>=iv)
|_ -> raise (TypeError "Type is incorrect"))

|NotEqual(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Bool_Val bx,Bool_Val bv) -> Bool_Val (bx<>bv)
|(Int_Val ix,Int_Val iv) -> Bool_Val (ix<>iv)
|_ -> raise (TypeError "Type is incorrect"))

|Equal(dl1,dl2) -> let n1=eval_expr env dl1 in
let n2=eval_expr env dl2 in
(match (n1,n2) with 
|(Bool_Val bx,Bool_Val bv) -> Bool_Val (bx=bv)
|(Int_Val ix,Int_Val iv) -> Bool_Val (ix=iv)
|_ -> raise (TypeError "Type is incorrect"))








let rec eval_stmt env s =
match s with 
|NoOp -> env

|Seq(se1,se2) -> eval_stmt (eval_stmt env se1) se2

|Declare(de1,se) -> (if (List.mem_assoc se env) then raise (DeclareError "the variable is already present")
else match de1 with
|Int_Type  ->(se, Int_Val 0)::env
|Bool_Type  -> (se,Bool_Val false)::env)

|Assign(se,ae1) -> let env2=eval_expr env ae1 in
if (List.mem_assoc se env) then 
(match (List.assoc se env, env2) with
|(Int_Val ih, Int_Val ig) -> ((se,Int_Val ig)::(List.remove_assoc se env))
|(Bool_Val bh, Bool_Val bg) -> ((se,Bool_Val bg)::(List.remove_assoc se env))
|_ -> raise (TypeError "not working"))
else
raise (DeclareError "the variable is not present")

|If(gd,ifs,elses) -> let n=eval_expr env gd in
(match n with 
|Bool_Val deci -> if deci then eval_stmt env ifs else eval_stmt env elses
|_ -> raise (TypeError "not working"))

|While(gd,bd) -> let n=eval_expr env gd in
(match n with 
|Bool_Val true ->eval_stmt (eval_stmt env bd)(While(gd,bd))
|Bool_Val false -> env
|_ -> raise (TypeError "not working"))


  |For (id, sexp, eexp, s2) -> 
    let n1 = eval_expr env sexp in
    let n2 = eval_expr env eexp in
    (match (n1,n2) with
    |(Int_Val f1,Int_Val f2) -> let as1=(eval_stmt env (Assign (id,sexp))) in
    if(f1<=f2) then let as2=(eval_stmt as1 s2) in
    let  as3=List.assoc id as2 in
    (match as3 with
    Int_Val j -> eval_stmt as2 (For (id, Int (j+1), eexp, s2))
    |Bool_Val j -> raise (TypeError "not working"))
    else as1
  | (Bool_Val b,Bool_Val d)-> raise (TypeError "not working")
  | ((Bool_Val f, Int_Val g)|(Int_Val g, Bool_Val f)) -> raise (TypeError "not working"))
  
  |Print(pr) -> match (eval_expr env pr) with
  |Int_Val iv -> (print_output_int iv;print_output_newline();env)
  |Bool_Val bv -> (print_output_bool bv;print_output_newline();env)


  
 
