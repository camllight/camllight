(* $Id$ *)

exception Error of string;;
type 'a option = None | Some of 'a;;
value init_env : string list;;
