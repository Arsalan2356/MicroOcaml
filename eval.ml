open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
  | Value(x) -> x

  | ID(x) -> lookup env x

  | Not(x) ->
    begin
    let v = eval_expr env x in
    match v with
    | Bool(x) -> Bool(not x)
    | _ -> raise (TypeError("Expected type bool in Not"))
    end
  
  | Binop(op, x, y) -> 
    begin
      match op with
      | Add -> 
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in

        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) -> Int(x + y)
          | _ -> raise(TypeError("Expected type int in Add"))
          )
        | _ -> raise(TypeError("Expected type int in Add"))
        )
      
      | Sub -> 
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in

        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) -> Int(x - y)
          | _ -> raise(TypeError("Expected type int in Sub"))
          )
        | _ -> raise(TypeError("Expected type int in Sub"))
        )
      
      | Mult -> 
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in

        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) -> Int(x * y)
          | _ -> raise(TypeError("Expected type int in Mult"))
          )
        | _ -> raise(TypeError("Expected type int in Mult"))
        )

      | Div -> 
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in

        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) when y = 0 -> raise(DivByZeroError)
          | Int(y) -> Int(x / y)
          | _ -> raise(TypeError("Expected type int in Mult"))
          )
        | _ -> raise(TypeError("Expected type int in Mult"))
        )

      | Greater ->
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in
        
        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) -> if x > y then Bool(true) else Bool(false)
          | _ -> raise(TypeError("Expected type int in Greater"))
          )
        | _ -> raise(TypeError("Expected type int in Greater"))
        )


      | GreaterEqual ->
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in
        
        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) -> if x >= y then Bool(true) else Bool(false)
          | _ -> raise(TypeError("Expected type int in GreaterEqual"))
          )
        | _ -> raise(TypeError("Expected type int in GreaterEqual"))
        )

      | Less ->
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in
        
        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) -> if x < y then Bool(true) else Bool(false)
          | _ -> raise(TypeError("Expected type int in Less"))
          )
        | _ -> raise(TypeError("Expected type int in Less"))
        )


      | LessEqual ->
        (
        let num1 = eval_expr env x in
        let num2 = eval_expr env y in
        
        match num1 with
        | Int(x) -> 
          (
          match num2 with
          | Int(y) -> if x <= y then Bool(true) else Bool(false)
          | _ -> raise(TypeError("Expected type int in LessEqual"))
          )
        | _ -> raise(TypeError("Expected type int in LessEqual"))
        )
  

      | Concat -> 
        (
        let str1 = eval_expr env x in
        let str2 = eval_expr env y in

        match str1 with
        | String(x) -> 
          (
          match str2 with
          | String(y) -> String(x ^ y)
          | _ -> raise(TypeError("Expected type String in Concat"))
          )
        | _ -> raise(TypeError("Expected type String in Concat"))

        )


      | Equal -> 
        (
        let str1 = eval_expr env x in
        let str2 = eval_expr env y in

        match str1, str2 with
        | Int(a), Int(b) -> if a = b then Bool(true) else Bool(false)
        | String(a), String(b) -> if a = b then Bool(true) else Bool(false)
        | Bool(a), Bool(b) -> if a = b then Bool(true) else Bool(false)
        | _, _ -> raise(TypeError("Cannot compare these types"))
        )

      | NotEqual ->
        (
        let str1 = eval_expr env x in
        let str2 = eval_expr env y in

        match str1, str2 with
        | Int(a), Int(b) -> if a = b then Bool(false) else Bool(true)
        | String(a), String(b) -> if a = b then Bool(false) else Bool(true)
        | Bool(a), Bool(b) -> if a = b then Bool(false) else Bool(true)
        | _, _ -> raise(TypeError("Cannot compare these types"))
        )
    

      | Or -> 
        (
        let bool1 = eval_expr env x in
        let bool2 = eval_expr env y in

        match bool1, bool2 with
        | Bool(a), Bool(b) -> Bool(a || b)
        | _, _ -> raise(TypeError("Expected type bool"))
        )

      | And -> 
        (
        let bool1 = eval_expr env x in
        let bool2 = eval_expr env y in

        match bool1, bool2 with
        | Bool(a), Bool(b) -> Bool(a && b)
        | _, _ -> raise(TypeError("Expected type bool"))
        )
    end

  | If(cond, x, y) -> 
    (
      let condition = eval_expr env cond in

      match condition with
      | Bool(a) -> 
        (
        match a with
        | true -> let evalx = eval_expr env x in evalx
        | false -> let evaly = eval_expr env y in evaly          
        )
      | _ -> raise(TypeError("Condition expected in If statement"))
    )

  | Let(v, r, x, y) -> 
    (
      match r with
      | false ->
        (
          let init_val = eval_expr env x in
          let env2 = extend env v init_val in
          eval_expr env2 y

        )


      | true ->
        (
          let env2 = extend_tmp env v in
          let init_val = eval_expr env2 x in
          let () = update env2 v init_val in
          eval_expr env2 y 
        )

    )

  | Fun(v, e) -> Closure(env, v, e)

  | FunctionCall(f, v) -> 
    let f_closure = eval_expr env f in
    let v_val = eval_expr env v in

    match f_closure with
    | Closure(e, x, y) -> 
      (
        let env2 = extend e x v_val in
        eval_expr env2 y
      )
    | _ -> raise(TypeError("Expected function in function call"))


  

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  match m with
  | Def(v, e) -> 
    (
      let env2 = extend_tmp env v in
      let e1 = eval_expr env2 e in
      let () = update env2 v e1 in
      (env2, Some (e1))
    )

  | Expr(e) -> (env, Some (eval_expr env e))

  | NoOp -> (env, None)