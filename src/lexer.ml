open TokenTypes
open Str
open String

let tru = Str.regexp"\\(true[^a-zA-Z0-9]\\)"
let fals = Str.regexp"\\(false[^a-zA-Z0-9]\\)"

let tokenize input =
 let length = String.length input in
 let rec tokhelper pos=
  if pos >= length then
  [EOF]
  else if (Str.string_match(Str.regexp"(") input pos) then
  Tok_LParen :: (tokhelper(pos+1))
  else if (Str.string_match(Str.regexp")") input pos) then
  Tok_RParen :: (tokhelper(pos+1))  
  else if (Str.string_match(Str.regexp"{") input pos) then
  Tok_LBrace :: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp"}") input pos) then
  Tok_RBrace :: (tokhelper(pos+1))
  else if (Str.string_match(Str.regexp"==") input pos) then
  Tok_Equal :: (tokhelper(pos+2))  
  else if (Str.string_match(Str.regexp"!=") input pos) then
  Tok_NotEqual:: (tokhelper(pos+2))
  else if (Str.string_match(Str.regexp"=") input pos) then
  Tok_Assign :: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp">") input pos) then
  Tok_Greater :: (tokhelper(pos+1))
  else if (Str.string_match(Str.regexp"<") input pos) then
  Tok_Less:: (tokhelper(pos+1))
  else if (Str.string_match(Str.regexp">=") input pos) then
  Tok_GreaterEqual:: (tokhelper(pos+2)) 
  else if (Str.string_match(Str.regexp"<=") input pos) then
  Tok_LessEqual:: (tokhelper(pos+2))   
  else if (Str.string_match(Str.regexp"||") input pos) then
  Tok_Or:: (tokhelper(pos+2)) 
  else if (Str.string_match(Str.regexp"&&") input pos) then
  Tok_And:: (tokhelper(pos+2)) 
  else if (Str.string_match(Str.regexp"!") input pos) then
  Tok_Not:: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp";") input pos) then
  Tok_Semi:: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp"int[^a-zA-Z0-9]") input pos) then
  Tok_Int_Type:: (tokhelper(pos+3)) 
  else if (Str.string_match(Str.regexp"bool[^a-zA-Z0-9]") input pos) then
  Tok_Bool_Type:: (tokhelper(pos+4)) 
  else if (Str.string_match(Str.regexp"printf[^a-zA-Z0-9]") input pos) then
  Tok_Print:: (tokhelper(pos+6)) 
  else if (Str.string_match(Str.regexp"main[^a-zA-Z0-9]") input pos) then
  Tok_Main :: (tokhelper(pos+4)) 
  else if (Str.string_match(Str.regexp"if[^a-zA-Z0-9]") input pos) then
  Tok_If :: (tokhelper(pos+2)) 
  else if (Str.string_match(Str.regexp"else[^a-zA-Z0-9]") input pos) then
  Tok_Else :: (tokhelper(pos+4)) 
  else if (Str.string_match(Str.regexp"for[^a-zA-Z0-9]") input pos) then
  Tok_For :: (tokhelper(pos+3)) 
  else if (Str.string_match(Str.regexp"from[^a-zA-Z0-9]") input pos) then
  Tok_From :: (tokhelper(pos+4)) 
  else if (Str.string_match(Str.regexp"to[^a-zA-Z0-9]") input pos) then
  Tok_To :: (tokhelper(pos+2)) 
  else if (Str.string_match(Str.regexp"while[^a-zA-Z0-9]") input pos) then
  Tok_While :: (tokhelper(pos+5))
  else if (Str.string_match(Str.regexp "\\+") input pos) then
  Tok_Add :: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp "\\*") input pos) then
  Tok_Mult :: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp "\\(/\\)") input pos) then
  Tok_Div :: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp "\\^") input pos) then
  Tok_Pow :: (tokhelper(pos+1)) 
  else if (Str.string_match(Str.regexp"-?[0-9]+") input pos) then
  let matched=Str.matched_string input in
  Tok_Int(int_of_string matched) :: (tokhelper(pos + (String.length matched)))
  else if (Str.string_match(Str.regexp "-") input pos) then
  Tok_Sub :: (tokhelper(pos+1)) 
  
  else if (Str.string_match(tru) input pos) then
  Tok_Bool(true) :: (tokhelper(pos+4)) 

  else if (Str.string_match(fals) input pos) then
  Tok_Bool(false) :: (tokhelper(pos+5)) 
  else if (Str.string_match(Str.regexp"[a-zA-Z][a-zA-Z0-9]*") input pos) then
  let matched=Str.matched_string input in
  Tok_ID(matched) :: (tokhelper(pos + (String.length matched)))
  else if (Str.string_match(Str.regexp"[ \n\t]") input pos) then
   (tokhelper(pos+(String.length(Str.matched_string input)))) 
  else raise (InvalidInputException "Doesn't work")
 in tokhelper 0 ;;
  