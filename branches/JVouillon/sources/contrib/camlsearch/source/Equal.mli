#open "myTypes";;
#open "external_type";;

value filter_iso_to :
   skel_typ -> string * GLOBAL_NAME * (int * int * EXTERNAL_TYPE) -> unit;;
