type 'a Queue;;

exception Empty;;

value empty : 'a Queue;;

value is_empty : 'a Queue -> bool;;

value insert : 'a Queue -> 'a -> 'a Queue;;

value insert_front : 'a Queue -> 'a -> 'a Queue;;

value remove : 'a Queue -> 'a * 'a Queue;;
