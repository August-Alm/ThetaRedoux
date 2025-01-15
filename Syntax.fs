module Theta.Syntax

open Theta.Core
open Theta.Tokens

type Syntax =
  /// x -- variable
  | SVar of string
  /// λx.t -- lambda abstraction
  | SLam of string * Syntax
  /// (t u) -- application
  | SApp of Syntax * Syntax
  /// θx.t -- theta abstraction
  | STht of string * Syntax
  /// [t u] -- annotation
  | SAnn of Syntax * Syntax
  /// μx.t -- fixpoint binder
  | SFix of string * Syntax
  /// let x = t; u -- let binding
  | SLet of string * Syntax * Syntax

[<RequireQualifiedAccess>]
module Syntax =

  let rec show (trm : Syntax) =
    match trm with
    | SVar x -> x
    | SLam (x, t) -> $"λ{x}.{show t}"
    | SApp (t, u) -> $"({show t} {show u})"
    | STht (x, t) -> $"θ{x}.{show t}"
    | SAnn (t, u) -> $"[{show t} {show u}]"
    | SFix (x, t) -> $"μ{x}.{show t}"
    | SLet (x, t, u) -> $"let {x} = {show t};\n{show u}"
  
  let term (trm : Syntax) =
    let rec go (xs : string list) trm =
      match trm with
      | SVar x ->
        match List.tryFindIndex ((=) x) xs with
        | Some i -> Var (x, i)
        | None -> failwith $"unbound variable {x}"
      | SLam (x, t) -> Lam (x, go (x :: xs) t)
      | SApp (t, u) -> App (go xs t, go xs u)
      | STht (x, t) -> Tht (x, go (x :: xs) t)
      | SAnn (t, u) -> Ann (go xs t, go xs u)
      | SFix (x, t) -> Fix (x, go (x :: xs) t)
      | SLet (x, t, u) -> go xs (SApp (SLam (x, u), t))
    go [] trm
  
  let private consume (tok : Token) (tokens : (Position * Token) list) =
    match tokens with
    | (_, tok') :: toks when tok = tok' -> toks
    | (pos, tok') :: _ -> error pos $"expected {tok} but got {tok'}"
    | [] -> failwith $"expected {tok} but reached end of input"
  
  let rec private read (tokens : (Position * Token) list) =
    match tokens with
    | (_, T_Var x) :: toks -> SVar x, toks
    | (_, T_Lam) :: (_, T_Var x) :: (_, T_Dot) :: toks ->
      let t, toks = read toks
      SLam (x, t), toks
    | (_, T_Tht) :: (_, T_Var x) :: (_, T_Dot) :: toks ->
      let t, toks = read toks
      STht (x, t), toks
    | (_, T_Fix) :: (_, T_Var x) :: (_, T_Dot) :: toks ->
      let t, toks = read toks
      SFix (x, t), toks
    | (_, T_LPar) :: toks ->
      let t, toks = read toks
      let u, toks = read toks
      SApp (t, u), consume T_RPar toks
    | (_, T_LBra) :: toks ->
      let t, toks = read toks
      let u, toks = read toks
      SAnn (t, u), consume T_RBra toks
    | (_, T_Let) :: (_, T_Var x) :: (_, T_Eq) :: toks ->
      let t, toks = read toks
      let toks = consume T_Sec toks
      let u, toks = read toks
      SLet (x, t, u), toks
    | (pos, tok) :: _ -> error pos $"unexpected token {tok}"
    | [] -> failwith "unexpected end of input"
  
  let parse (inp : Input) = fst (read (tokenize inp))

  let parseString = parse << InputOfString
