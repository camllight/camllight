type type_simple and schéma_de_types;;

value type_int: type_simple
  and type_bool: type_simple
  and type_flèche: type_simple -> type_simple -> type_simple
  and type_produit: type_simple -> type_simple -> type_simple
  and type_liste: type_simple -> type_simple;;

value nouvelle_inconnue: unit -> type_simple
  and unifie: type_simple -> type_simple -> unit
  and généralisation: type_simple -> schéma_de_types
  and spécialisation: schéma_de_types -> type_simple
  and schéma_trivial: type_simple -> schéma_de_types
  and début_de_définition: unit -> unit
  and fin_de_définition: unit -> unit;;

exception Conflit of type_simple * type_simple
      and Circularité of type_simple * type_simple;;

value imprime_type: type_simple -> unit
  and imprime_schéma: schéma_de_types -> unit;;
