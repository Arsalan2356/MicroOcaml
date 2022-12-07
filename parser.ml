open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  let next_token = lookahead toks in
  match next_token with
  | Some Tok_Let ->  
    let (tokens1, e1) = parse_let toks in
    (tokens1, e1)

  | Some Tok_If -> 
    let (tokens1, e1) = parse_if toks in
    (tokens1, e1)

  | Some Tok_Fun ->
    let (tokens1, e1) = parse_function toks in
    (tokens1, e1)
    
  | Some Tok_Not | Some Tok_Int(_) | Some Tok_Bool(_) |
    Some Tok_String(_) | Some Tok_ID(_) | Some Tok_LParen
  -> let (tokens1, e1) = parse_or toks in
    (tokens1, e1)

  | _ -> raise (InvalidInputException "Parsing Error")

and parse_let toks = 
  let recursive = lookahead_many toks 1 in
  let id = lookahead_many toks 2 in
  let rec_bool = match recursive with
  | Some Tok_Rec -> true
  | _ -> false
  in
  if (rec_bool) then
    let let_id = match id with
                | Some Tok_ID(x) -> x
                | _ -> ""
    in
    Format.print_bool(rec_bool);
    let new_tokens = match_many toks [Tok_Let; Tok_Rec; Tok_ID(let_id); Tok_Equal] in
    let (e1_tokens, e1) = parse_expr new_tokens in
    let tokens_to_e2 = match_token e1_tokens Tok_In in
    let (e2_tokens, e2) = parse_expr tokens_to_e2 in
    (e2_tokens, Let(let_id, true, e1, e2))
  else
    let id = lookahead_many toks 1 in
    let let_id = match id with
                | Some Tok_ID(x) -> x
                | _ -> ""
    in
    let new_tokens = match_many toks [Tok_Let; Tok_ID(let_id); Tok_Equal] in
    let (e1_tokens, e1) = parse_expr new_tokens in
    let tokens_to_e2 = match_token e1_tokens Tok_In in
    let (e2_tokens, e2) = parse_expr tokens_to_e2 in
    (e2_tokens, Let(let_id, false, e1, e2))

and parse_if toks =
  let tokens1 = match_token toks Tok_If in
  let (tokens2, e1) = parse_expr tokens1 in
  let tokens3 = match_token tokens2 Tok_Then in
  let (tokens4, e2) = parse_expr tokens3 in
  let tokens5 = match_token tokens4 Tok_Else in
  let (tokens6, e3) = parse_expr tokens5 in
  (tokens6, If(e1, e2, e3))


and parse_function toks = 
  let id = lookahead_many toks 1 in
  let fun_id = match id with
    | Some Tok_ID(x) -> x
    | _ -> ""
  in 
  let tokens1 = match_many toks [Tok_Fun; Tok_ID(fun_id); Tok_Arrow] in
  let (tokens2, e1) = parse_expr tokens1 in
  (tokens2, Fun(fun_id, e1))

and parse_or toks = 
  let (tokens1, e1) = parse_and toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_Or -> 
    let tokens2 = match_token tokens1 Tok_Or in
    let (tokens3, e2) = parse_or tokens2 in
    (tokens3, Binop(Or, e1, e2))
  | _ -> (tokens1, e1)

and parse_and toks = 
  let (tokens1, e1) = parse_equality toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_And ->  
    let tokens2 = match_token tokens1 Tok_And in
    let (tokens3, e2) = parse_and tokens2 in
    (tokens3, Binop(And, e1, e2))
  | _ -> (tokens1, e1)



and parse_equality toks = 
  let (tokens1, e1) = parse_relational toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_Equal -> 
      let tokens2 = match_token tokens1 Tok_Equal in
      let (tokens3, e2) = parse_equality tokens2 in
      (tokens3, Binop(Equal, e1, e2))

  | Some Tok_NotEqual ->
    let tokens2 = match_token tokens1 Tok_NotEqual in
    let (tokens3, e2) = parse_equality tokens2 in
    (tokens3, Binop(NotEqual, e1, e2))
  | _ -> (tokens1, e1)


and parse_relational toks = 
  let (tokens1, e1) = parse_additive toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_Less ->
    let tokens2 = match_token tokens1 Tok_Less in
    let (tokens3, e2) = parse_relational tokens2 in
    (tokens3, Binop(Less, e1, e2))
  | Some Tok_Greater ->
    let tokens2 = match_token tokens1 Tok_Greater in
    let (tokens3, e2) = parse_relational tokens2 in
    (tokens3, Binop(Greater, e1, e2))
  | Some Tok_LessEqual ->
    let tokens2 = match_token tokens1 Tok_LessEqual in
    let (tokens3, e2) = parse_relational tokens2 in
    (tokens3, Binop(LessEqual, e1, e2))
  | Some Tok_GreaterEqual ->
    let tokens2 = match_token tokens1 Tok_GreaterEqual in
    let (tokens3, e2) = parse_relational tokens2 in
    (tokens3, Binop(GreaterEqual, e1, e2))
  | _ -> (tokens1, e1)




and parse_additive toks = 
  let (tokens1, e1) = parse_multiplicative toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_Add -> 
    let tokens2 = match_token tokens1 Tok_Add in
    let (tokens3, e2) = parse_additive tokens2 in
    (tokens3, Binop(Add, e1, e2))
  | Some Tok_Sub -> 
    let tokens2 = match_token tokens1 Tok_Sub in
    let (tokens3, e2) = parse_additive tokens2 in
    (tokens3, Binop(Sub, e1, e2))
    
  | _ -> (tokens1, e1)


and parse_multiplicative toks = 
  let (tokens1, e1) = parse_concat toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_Mult ->
    let tokens2 = match_token tokens1 Tok_Mult in
    let (tokens3, e2) = parse_multiplicative tokens2 in
    (tokens3, Binop(Mult, e1, e2))

  | Some Tok_Div -> 
    let tokens2 = match_token tokens1 Tok_Div in
    let (tokens3, e2) = parse_multiplicative tokens2 in
    (tokens3, Binop(Div, e1, e2))
  | _ -> (tokens1, e1)

and parse_concat toks = 
  let (tokens1, e1) = parse_unary toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_Concat ->
    let tokens2 = match_token tokens1 Tok_Concat in
    let (tokens3, e2) = parse_concat tokens2 in
    (tokens3, Binop(Concat, e1, e2))
  | _ -> (tokens1, e1)
    

and parse_unary toks = 
  let next_token = lookahead toks in
  match next_token with
  | Some Tok_Not -> 
    let tokens2 = match_token toks Tok_Not in
    let (tokens3, e1) = parse_unary tokens2 in
    (tokens3, Not(e1))
  | _ -> parse_function_call toks

and parse_function_call toks = 
  let (tokens1, e1) = parse_primary toks in
  let next_token = lookahead tokens1 in
  match next_token with
  | Some Tok_Int(_) | Some Tok_Bool(_) | Some Tok_String(_) | Some Tok_ID(_) |
   Some Tok_LParen -> 
    let (tokens2, e2) = parse_primary tokens1 in
    (tokens2, FunctionCall(e1, e2))

  | _ -> (tokens1, e1)


and parse_primary toks = 
  let next_token = lookahead toks in
  match next_token with
  | Some Tok_Int(x) -> 
    let tokens1 = match_token toks (Tok_Int(x)) in
    (tokens1, Value(Int(x)))

  | Some Tok_Bool(x) ->
    let tokens1 = match_token toks (Tok_Bool(x)) in
    (tokens1, Value(Bool(x)))

  | Some Tok_String(x) ->
    let tokens1 = match_token toks (Tok_String(x)) in
    (tokens1, Value(String(x)))

  | Some Tok_ID(x) ->
    let tokens1 = match_token toks (Tok_ID(x)) in
    (tokens1, ID(x))

  | Some Tok_LParen -> 
    let tokens1 = match_token toks Tok_LParen in
    let (tokens2, e1) = parse_expr tokens1 in
    let tokens3 = match_token tokens2 Tok_RParen in
    (tokens3, e1)

  | _ -> raise (InvalidInputException "Parsing Error")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  let next_token = lookahead toks in
  match next_token with
  | Some Tok_Def -> 
    let (tokens1, e1) = parse_def toks in
    (tokens1, e1)

  | Some Tok_DoubleSemi ->
    let tokens1 = match_token toks Tok_DoubleSemi in
    (tokens1, NoOp)

  | _ -> 
    let (tokens1, e1) = parse_mutop_expr toks in
    (tokens1, e1)

and parse_def toks = 
  let id = lookahead_many toks 1 in
  let def_id = match id with
              | Some Tok_ID(x) -> x
              | _ -> ""
  in
  let tokens1 = match_many toks [Tok_Def; Tok_ID(def_id); Tok_Equal] in
  let (tokens2, e1) = parse_expr tokens1 in
  let tokens3 = match_token tokens2 Tok_DoubleSemi in
  (tokens3, Def(def_id, e1))



and parse_mutop_expr toks = 
  let (tokens1, e1) = parse_expr toks in
  let tokens2 = match_token tokens1 Tok_DoubleSemi in
  (tokens2, Expr(e1))
