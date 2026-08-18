open Ast

(* Program file Support *)
type stmt =
  | Eval of expr
  | Import of string
  | Def of string * expr