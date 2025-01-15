module Theta.Parse


open Theta.Tokens
open Theta.Syntax
open Theta.Core

let consume (tok : Token) (tokens : (Position * Token) list) =
  match tokens with
  | (_, tok') :: toks' when tok = tok' -> toks'
  | (pos, tok') :: _ -> error pos $"expected {tok} but got {tok'}"
  | [] -> tokens