(* Base types and constructors *)

(* This module defines some types and exceptions for which the language
   provides special syntax, and are therefore treated specially
   by the compiler. *)

type int;;
type float;;
type string;;
type char;;
        (* The types of integers, floating-point numbers, character strings,
           and characters, respectively. *)

type exn;;
        (* The type of exception values. *)

type bool = false | true;;
        (* The type of boolean values. *)

type 'a vect;;
        (* The type of arrays whose elements have type ['a]. *)

type unit = ();;
        (* The type of the unit value. *)

type 'a list = [] | prefix :: of 'a * 'a list;;
        (* The type of lists. *)

exception Match_failure of string * int * int;;
        (* The exception raised when a pattern-matching fails.
           The argument indicates the position in the source code of the
           pattern-matching (source file name, position of the first
           character of the matching, position of the last character. *)
