type type_simple and sch�ma_de_types;;

value type_int: type_simple
  and type_bool: type_simple
  and type_fl�che: type_simple -> type_simple -> type_simple
  and type_produit: type_simple -> type_simple -> type_simple
  and type_liste: type_simple -> type_simple;;

value nouvelle_inconnue: unit -> type_simple
  and unifie: type_simple -> type_simple -> unit
  and g�n�ralisation: type_simple -> sch�ma_de_types
  and sp�cialisation: sch�ma_de_types -> type_simple
  and sch�ma_trivial: type_simple -> sch�ma_de_types
  and d�but_de_d�finition: unit -> unit
  and fin_de_d�finition: unit -> unit;;

exception Conflit of type_simple * type_simple
      and Circularit� of type_simple * type_simple;;

value imprime_type: type_simple -> unit
  and imprime_sch�ma: sch�ma_de_types -> unit;;
