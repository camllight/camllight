type t;;

value empty: int -> t
  and singleton: int -> int -> t
  and union: t -> t -> t
  and min: t -> int
  and apply: (int -> 'a) -> t -> unit
;;
