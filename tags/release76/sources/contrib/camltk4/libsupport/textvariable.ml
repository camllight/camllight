#open "protocol";;

type textVariable == string
;;

let counter = ref 0
;;

let freelist = ref (set__empty compare_strings)
and memo = hashtblc__new 101
;;

(* Added a variable v referenced by widget w *)
let add w v =
  let r = 
    try hashtblc__find memo w 
    with
      Not_found -> 
      	let r = ref (set__empty compare_strings) in
	  hashtblc__add memo w r;
	  r in
   r := set__add v !r
;;


(* Free a widget *)
let free w =
  try
    let r = hashtblc__find memo w in
      freelist := set__union !freelist !r;
      hashtblc__remove memo w 
  with
    Not_found -> ()
;;

add_destroy_hook free
;;

(* Allocate a new variable *)
let getv () = 
  let v = 
    if set__is_empty !freelist then begin
      incr counter; 
      "camlv("^ string_of_int !counter ^")"
      end
    else
      let v = set__choose !freelist in
	freelist := set__remove v !freelist;
	v in
    tkDo [| TkToken "set"; TkToken v; TkToken "" |];
    v
;;

let create_temporary w =
  let v = getv() in
    add w v;
    v
;;

(* Initialize the variable to avoid error *)
let create () = getv ()
;;

let set v x =
  tkDo [| TkToken "set"; TkToken v; TkToken x |]
;;
let get v =
  tkEval [| TkToken "set"; TkToken v |]
;;

let cCAMLtoTKtextVariable s = TkToken s
;;

let name s = s
;;
