open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
  let re_rparen = Str.regexp ")" in
  let re_lparen = Str.regexp "(" in
  let re_equal = Str.regexp "=" in
  let re_notequal = Str.regexp "<>" in
  let re_greater = Str.regexp ">" in
  let re_less = Str.regexp "<" in
  let re_greater_equal = Str.regexp ">=" in
  let re_less_equal = Str.regexp "<=" in
  let re_or = Str.regexp "||" in
  let re_and = Str.regexp "&&" in
  let re_not = Str.regexp "not" in
  let re_if = Str.regexp "if" in
  let re_then = Str.regexp "then" in
  let re_else = Str.regexp "else" in
  let re_add = Str.regexp "\\+" in
  let re_sub = Str.regexp "-" in
  let re_mult = Str.regexp "\\*" in
  let re_div = Str.regexp "/" in
  let re_concat = Str.regexp "\\^" in
  let re_let = Str.regexp "let" in
  let re_rec = Str.regexp "rec" in
  let re_in = Str.regexp "in" in
  let re_def = Str.regexp "def" in 
  let re_fun = Str.regexp "fun" in 
  let re_arrow = Str.regexp "->" in 
  let re_int = Str.regexp "[0-9]+" in
  let re_negative_int = Str.regexp "(\\(-[0-9]+\\))" in
  let re_bool = Str.regexp "true\\|false" in
  let re_string = Str.regexp "\"\\([^\"]*\\)\"" in
  let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let re_double_semi = Str.regexp ";;" in
  let re_skip = Str.regexp "[ \t\n]+" in
  let re_any = Str.regexp "[a-zA-Z0-9]+" in

  let rec next_token str pos =

    if (pos >= String.length str) then
      []
  
    else if (Str.string_match re_string str pos) then
        let token = Str.matched_group 1 str in
        let new_pos = Str.match_end() in
        (Tok_String (token))::(next_token str (new_pos))

    else if (Str.string_match re_int str pos) then
      let token = Str.matched_string str in
      let new_pos = Str.match_end() in 

      (Tok_Int (int_of_string token))::(next_token str new_pos)

    else if (Str.string_match re_negative_int str pos) then
      let token = Str.matched_group 1 str in
      let new_pos = Str.match_end() in

      (Tok_Int (int_of_string token))::(next_token str new_pos)

    else if (Str.string_match re_bool str pos) then
      let token = Str.matched_string str in 
      let new_pos = Str.match_end() in 

      (Tok_Bool (bool_of_string token))::(next_token str new_pos)

    else if (Str.string_match re_less_equal str pos) then
      (Tok_LessEqual)::(next_token str (pos + 2))

    else if (Str.string_match re_arrow str pos) then
      (Tok_Arrow)::(next_token str (pos + 2))

    else if (Str.string_match re_greater_equal str pos) then
      (Tok_GreaterEqual)::(next_token str (pos + 2))

    else if (Str.string_match re_notequal str pos) then
      (Tok_NotEqual)::(next_token str (pos + 2))

    else if (Str.string_match re_lparen str pos) then
      (Tok_LParen)::(next_token str (pos + 1))

    else if (Str.string_match re_rparen str pos) then
      (Tok_RParen)::(next_token str (pos + 1))
    
    else if (Str.string_match re_equal str pos) then
      (Tok_Equal)::(next_token str (pos + 1))

    else if (Str.string_match re_greater str pos) then
      (Tok_Greater)::(next_token str (pos + 1))

    else if (Str.string_match re_less str pos) then
      (Tok_Less)::(next_token str (pos + 1))

    else if (Str.string_match re_or str pos) then
      (Tok_Or)::(next_token str (pos + 2))

    else if (Str.string_match re_and str pos) then
      (Tok_And)::(next_token str (pos + 2))
    
    else if (Str.string_match re_not str pos) then
      if (Str.string_match re_any str (pos + 3)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("not"^token))::(next_token str (new_pos))

      else (Tok_Not)::(next_token str (pos + 3))

    else if (Str.string_match re_if str pos) then
      if (Str.string_match re_any str (pos + 2)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("if"^token))::(next_token str (new_pos))
      
      else (Tok_If)::(next_token str (pos + 2))
    
    else if (Str.string_match re_then str pos) then
      if (Str.string_match re_any str (pos + 4)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("then"^token))::(next_token str (new_pos))

      else (Tok_Then)::(next_token str (pos + 4))
    
    else if (Str.string_match re_else str pos) then
      if (Str.string_match re_any str (pos + 4)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("else"^token))::(next_token str (new_pos))

      else (Tok_Else)::(next_token str (pos + 4))
    
    else if (Str.string_match re_add str pos) then
      (Tok_Add)::(next_token str (pos + 1))

    else if (Str.string_match re_sub str pos) then
      (Tok_Sub)::(next_token str (pos + 1))
    
    else if (Str.string_match re_mult str pos) then
      (Tok_Mult)::(next_token str (pos + 1))

    else if (Str.string_match re_div str pos) then
      (Tok_Div)::(next_token str (pos + 1))
    
    else if (Str.string_match re_concat str pos) then
      (Tok_Concat)::(next_token str (pos + 1))

    else if (Str.string_match re_let str pos) then
      if (Str.string_match re_any str (pos + 3)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("let"^token))::(next_token str (new_pos))

      else (Tok_Let)::(next_token str (pos + 3))
    
    else if (Str.string_match re_rec str pos) then
      if (Str.string_match re_any str (pos + 3)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("rec"^token))::(next_token str (new_pos))
      
      else (Tok_Rec)::(next_token str (pos + 3))
    
    else if (Str.string_match re_in str pos) then
      if (Str.string_match re_any str (pos + 2)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("in"^token))::(next_token str (new_pos))

      else (Tok_In)::(next_token str (pos + 2))
    
    else if (Str.string_match re_def str pos) then
      if (Str.string_match re_any str (pos + 3)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("def"^token))::(next_token str (new_pos))
      
      else (Tok_Def)::(next_token str (pos + 3))
    
    else if (Str.string_match re_fun str pos) then
      if (Str.string_match re_any str (pos + 3)) then
        let token = Str.matched_string str in
        let new_pos = Str.match_end() in
        (Tok_ID ("fun"^token))::(next_token str (new_pos))
      
      else (Tok_Fun)::(next_token str (pos + 3))
    

    else if (Str.string_match re_double_semi str pos) then
      (Tok_DoubleSemi)::(next_token str (pos + 2))
    
    else if (Str.string_match re_id str pos) then
      let token = Str.matched_string str in 
      let new_pos = Str.match_end() in 
      (Tok_ID (token))::(next_token str new_pos)

    
    else if (Str.string_match re_skip str pos) then
      (next_token str (Str.match_end()))

    else
      raise (InvalidInputException input)


  in next_token input 0;;