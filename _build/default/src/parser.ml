open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))





(*Expr -> OrExpr
OrExpr -> AndExpr || OrExpr | AndExpr
AndExpr -> EqualityExpr && AndExpr | EqualityExpr
EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
EqualityOperator -> == | !=
RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
RelationalOperator -> < | > | <= | >=
AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
AdditiveOperator -> + | -
MultiplicativeExpr -> PowerExpr MultiplicativeOperator MultiplicativeExpr | PowerExpr
MultiplicativeOperator -> * | /
PowerExpr -> UnaryExpr ^ PowerExpr | UnaryExpr
UnaryExpr -> ! UnaryExpr | PrimaryExpr
PrimaryExpr -> Tok_Int | Tok_Bool | Tok_ID | ( Expr )*)

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  match toks with 
  [EOF] -> raise (InvalidInputException("wrong"))
|_ -> let (remaining_tokens,expr)=parse_or toks in
     (remaining_tokens,expr)

and parse_or toks =
    let (toks2,expr1) = (parse_and toks) in
    match lookahead toks2 with
    Tok_Or -> let toks3=match_token toks2 Tok_Or in
    let (toks4,expr2)=parse_or toks3 in
     (toks4, Or (expr1,expr2))
    | _ -> (toks2,expr1)

   
and parse_and toks =
let (toks2,expr1) = (parse_eqexp toks) in
match lookahead toks2 with
Tok_And -> let toks3=match_token toks2 Tok_And in
let (toks4,expr2)=parse_and toks3 in
 (toks4, And(expr1,expr2))
| _ -> (toks2,expr1)



and parse_eqexp toks =
let (toks2,expr) = (parse_relat toks) in
 let l= lookahead toks2 in
 if(l=Tok_Equal || l=Tok_NotEqual) then
 let toks3=match_token toks2 l in 
 let (toks4,expr2)=parse_eqexp toks3 in
 if l=Tok_Equal then (toks4,Equal(expr,expr2))
 else (toks4,NotEqual(expr,expr2))
 else
 (toks2,expr)


and parse_relat toks =
let (toks2,expr) = (parse_add toks) in
match lookahead toks2 with
|Tok_Less -> let toks3=match_token toks2 Tok_Less in
let (toks4,expr2)=parse_relat toks3 in
(toks4, Less(expr,expr2))
|Tok_LessEqual -> let toks3=match_token toks2 Tok_LessEqual in
let (toks4,expr2)=parse_relat toks3 in
(toks4, LessEqual(expr,expr2))
|Tok_Greater -> let toks3=match_token toks2 Tok_Greater in
let (toks4,expr2)=parse_relat toks3 in
(toks4, Greater(expr,expr2))
|Tok_GreaterEqual -> let toks3=match_token toks2 Tok_GreaterEqual in
let (toks4,expr2)=parse_relat toks3 in
(toks4, GreaterEqual(expr,expr2))
|_ -> (toks2,expr)


 
    
and parse_add toks =
let (toks2,expr) = (parse_mult toks) in
match lookahead toks2 with
|Tok_Add -> let toks3=match_token toks2 Tok_Add in
let (toks4,expr2)=parse_add toks3 in
(toks4, Add(expr,expr2))
|Tok_Sub -> let toks3=match_token toks2 Tok_Sub in
let (toks4,expr2)=parse_add toks3 in
(toks4, Sub(expr,expr2))
|_ -> (toks2,expr)



and parse_mult toks =
let (toks2,expr) = (parse_powe toks) in
match lookahead toks2 with
|Tok_Mult -> let toks3=match_token toks2 Tok_Mult in
let (toks4,expr2)=parse_mult toks3 in
(toks4, Mult(expr,expr2))
|Tok_Div -> let toks3=match_token toks2 Tok_Div in
let (toks4,expr2)=parse_mult toks3 in
(toks4, Div(expr,expr2))
| _ -> (toks2,expr)


and parse_powe toks =
let (toks2,expr) = (parse_un toks) in
match lookahead toks2 with
|Tok_Pow -> let toks3=match_token toks2 Tok_Pow in
let (toks4,expr2)=parse_powe toks3 in
(toks4, Pow(expr,expr2))
| _ -> (toks2,expr)

and parse_un toks =
(match lookahead toks with
| Tok_Not -> let toks2=match_token toks Tok_Not in
let (toks2,expr2)=parse_un toks2 in
(toks2, Not(expr2))
| _ -> parse_primary toks)
  
     
and parse_primary toks =
(match lookahead toks with
| Tok_Int i ->  let toks2=match_token toks (Tok_Int i) in
(toks2, Int i)
| Tok_Bool i ->  let toks2=match_token toks (Tok_Bool i) in
(toks2, Bool i)
|Tok_ID i ->  let toks2=match_token toks (Tok_ID i) in
(toks2, ID i)
|Tok_LParen  -> let toks2=match_token toks Tok_LParen in
let (toks3,expr2)=parse_expr toks2 in
let toks4 = match_token toks3 Tok_RParen in 
(toks4,expr2)
|_ -> raise (InvalidInputException"Expression is wrong"))

(*Stmt -> StmtOptions Stmt | ε
StmtOptions -> DeclareStmt | AssignStmt | PrintStmt | IfStmt | ForStmt | WhileStmt
DeclareStmt -> BasicType ID ;
BasicType -> int | bool
AssignStmt -> ID = Expr ;
PrintStmt -> printf ( Expr ) ;
IfStmt -> if ( Expr ) { Stmt } ElseBranch
ElseBranch -> else { Stmt } | ε
ForStmt -> for ( ID from Expr to Expr ) { Stmt }
WhileStmt -> while ( Expr ) { Stmt }*)


let rec parse_stmt toks : stmt_result =
    if (lookahead toks) = Tok_RBrace then (toks,NoOp)
    else if (lookahead toks) = EOF then (toks,NoOp)
    else
    let (toks1,expr1)= parse_options toks in 
match (lookahead toks1) with Tok_RBrace -> (toks1,Seq(expr1,NoOp))
|_ -> let (toks2,expr2) = parse_stmt toks1 in  (toks2,Seq(expr1,expr2))
  
    and parse_options toks =
    match lookahead toks with
    
    |Tok_Int_Type -> let toks1= match_token toks Tok_Int_Type in
    (match lookahead toks1 with
    |Tok_ID i -> let toks2= match_token toks1 (Tok_ID i) in
    let toks3=match_token toks2 Tok_Semi in
    (toks3,Declare(Int_Type,i))
    | _ -> raise (InvalidInputException "no id given in Int"))
  
    |Tok_Bool_Type -> let toks1= match_token toks Tok_Bool_Type in
    (match lookahead toks1 with
    |Tok_ID i -> let toks2= match_token toks1 (Tok_ID i) in
    let toks3=match_token toks2 Tok_Semi in
    (toks3,Declare(Bool_Type,i))
    | _ -> raise (InvalidInputException "no id given in Bool"))
  
  
    |Tok_ID i -> let toks1= match_token toks (Tok_ID i) in
     let toks2= match_token toks1 Tok_Assign in
     let (toks3,expr2)= parse_expr toks2 in
     let toks4= match_token toks3 Tok_Semi in
     (toks4,Assign(i,expr2))
  
  
  
     |Tok_Print -> let toks1= match_token toks Tok_Print in
     let toks2 = match_token toks1 Tok_LParen in
     let (toks3,expr2)=parse_expr toks2 in
     let toks4 = match_token toks3 Tok_RParen in
     let toks5= match_token toks4 Tok_Semi in
     (toks5,Print(expr2))
  
  
  
     |Tok_If -> let toks1= match_token toks Tok_If in
     let toks2= match_token toks1 Tok_LParen in
     let (toks3,expr2)=parse_expr toks2 in
     let toks4 = match_token toks3 Tok_RParen in
     let toks5 = match_token toks4 Tok_LBrace in
     let (toks6,expr3)= parse_stmt toks5 in 
     let toks7 = match_token toks6 Tok_RBrace in
     (match lookahead toks7 with
     |Tok_Else -> let toks8 = match_token toks7 Tok_Else in
     let toks9 = match_token toks8 Tok_LBrace in
     let (toks10,expr4)= parse_stmt toks9 in
     let toks11 = match_token toks10 Tok_RBrace in
     (toks11, If(expr2,expr3,expr4))
     |_ -> (toks7,If(expr2,expr3,NoOp)))
  
  
     |Tok_For -> 
     let toks1=match_token toks Tok_For in
     let toks2= match_token toks1 Tok_LParen in
     (match lookahead toks2 with
    |Tok_ID i -> let toks3= match_token toks2 (Tok_ID i) in
    let toks4 = match_token toks3 Tok_From in
    let (toks5,expr1) = parse_expr toks4 in 
    let toks6 = match_token toks5 Tok_To in
    let (toks7,expr2) = parse_expr toks6 in 
    let toks8 = match_token toks7 Tok_RParen in
    let toks9=match_token toks8 Tok_LBrace in
    let (toks10,expr3)=parse_stmt toks9 in
    let toks11=match_token toks10 Tok_RBrace in
    (toks11,For(i,expr1,expr2,expr3))
    |_ -> raise (InvalidInputException "no id given"))
  
  
  
    |Tok_While -> let toks1=match_token toks Tok_While in
    let toks2= match_token toks1 Tok_LParen in
    let (toks3,expr2)=parse_expr toks2 in
    let toks4 = match_token toks3 Tok_RParen in
    let toks5 = match_token toks4 Tok_LBrace in
     let (toks6,expr3)= parse_stmt toks5 in
     let toks7 = match_token toks6 Tok_RBrace in
     (toks7,While(expr2,expr3))
  
     |Tok_RBrace -> (toks,NoOp)
     |EOF -> (toks,NoOp)
     |_ -> raise (InvalidInputException "wrong expression")
  






  (*Main -> int main ( ) { Stmt } EOF*)
  
let parse_main toks : stmt =
  let tok = match_token toks Tok_Int_Type in
  let tok1 = match_token tok Tok_Main in
  let tok2 = match_token tok1 Tok_LParen in
  let tok3 = match_token tok2 Tok_RParen in
  let tok4 = match_token tok3 Tok_LBrace in
  let (tok_alt, x) = parse_stmt tok4 in 
  let tok_alt1 = match_token tok_alt Tok_RBrace in
  if tok_alt1 = [EOF] then x 
  else raise (InvalidInputException "End Of File Has Not Been Reached")