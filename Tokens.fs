module Theta.Tokens

open System.Text

[<AbstractClass>]
type Input () =
  abstract member Pop : unit -> int32
  abstract member Peek : unit -> int32

type InputOfString (str : string) =
  inherit Input ()
  let s = str
  let len = s.Length
  let mutable i = -1
  override _.Pop () =
    i <- i + 1; if i >= len then -1 else int s[i]
  override _.Peek () =
    let j = i + 1 in if j >= len then -1 else int s[j]

type InputOfStream (strm : System.IO.StreamReader) =
  inherit Input ()
  override _.Pop () = strm.Read ()
  override _.Peek () = strm.Peek () 

type Token =
  | T_Var of string 
  | T_Lam
  | T_Tht
  | T_Fix
  | T_Dot
  | T_Let
  | T_LPar
  | T_RPar
  | T_LBra
  | T_RBra
  | T_Eq
  | T_Sec
  | T_Eof

let inline isAlpha c = (c > 64 && c < 91) || (c > 96 && c < 123)
let inline isName c = isAlpha c || (c > 46 && c < 58) || c = int '_'
let inline isHash c = (c = int '#')
let inline isLF c = (c = int '\n')
let inline isSpace c = (c = int ' ' || c = int '\t' || c = int '\r' || isLF c)

let inline private append (c : int) (sb : StringBuilder) =
  sb.Append (char c) |> ignore

[<Struct>]
type Position = { Line : int; Column : int }

let inline error (pos : Position) msg =
  failwith (sprintf "At (%i, %i):\t%s" pos.Line pos.Column msg)

let tokenize (inp : Input) =
  let sb = StringBuilder 32
  let mutable line = 1
  let mutable column = 0

  let rec readChar () =
    let mutable c = inp.Pop ()
    if isHash c then
      while not (isLF c) && c <> -1 do
        c <- inp.Pop ()
    if isLF c then
      line <- line + 1
      column <- 0
      readChar ()
    else
      column <- column + 1
      c
  
  let readNonSpace () =
    let mutable c = readChar ()
    while isSpace c do c <- readChar ()
    c

  let readName (c : int) =
    let mutable c = c
    let mutable cnxt = inp.Peek ()
    while isName cnxt do
      append c sb
      c <- readChar ()
      cnxt <- inp.Peek ()
    append c sb
    let name = sb.ToString ()
    sb.Clear () |> ignore
    name
  
  let readToken () =
    let c = readNonSpace ()
    let pos = {Line = line; Column = column}
    if c = -1 then
      pos, T_Eof
    else
      match char c with
      | 'λ' -> pos, T_Lam
      | 'θ' -> pos, T_Tht
      | 'μ' -> pos, T_Fix
      | '.' -> pos, T_Dot 
      | '(' -> pos, T_LPar
      | ')' -> pos, T_RPar
      | '[' -> pos, T_LBra
      | ']' -> pos, T_RBra
      | ';' -> pos, T_Sec
      | '=' -> pos, T_Eq
      | _ ->
        if isAlpha c then
          match readName c with
          | "let" -> pos, T_Let
          | x -> pos, T_Var x
        else
          error pos "Invalid token."
  
  Seq.unfold (fun _ ->
    match readToken () with
    | _, T_Eof -> None
    | tok -> Some (tok, ())) ()
  |> Seq.toList

