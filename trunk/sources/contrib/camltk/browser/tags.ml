(* Emacs TAGS file support *)
let use_tags = ref false;;

let reverse_pos s c =
  let rec pos i =
    if i < 0 then raise Not_found
    else if nth_char s i == c then i
    else pos (i - 1)
  in pos (string_length s - 1)
;;

let reverse_sep s p =
  let rec pos i =
    if i < 0 then raise Not_found
    else if p (nth_char s i) then i
    else pos (i - 1)
  in pos (string_length s - 1)
;;

let separator_char = function
  ` ` | `\t` | `|` | `{` -> true
| _ -> false
;;


let rec read_symbols ic h =
 try
  let l = input_line ic in 
    if  l = "\012" then h
    else begin 
      	 let comma = reverse_pos l `,`
      	 and control = reverse_pos l `\127`
	 and symbol_beg = reverse_sep l separator_char in
	 let line_num = 
      	    int_of_string (sub_string l (succ control) (comma - control - 1))
         and symbol =
	    sub_string l (succ symbol_beg) (control - symbol_beg - 1) in
	  hashtbl__add h symbol (pred line_num) ;
      	 read_symbols ic h    
     end
 with
  End_of_file -> h
;;

let dotml_emacs_tags = hashtbl__new 17
;;

let dotmli_emacs_tags = hashtbl__new 17
;;

let read_tag_file ic =
 let _ = input_line ic in (* dump first line, assumed "\012" *)
  try
   while true do
      let file_line = input_line ic in
      let comma = reverse_pos file_line `,` in
      let file_name = sub_string file_line 0 comma in
      let mod_name, h = 
	 if filename__check_suffix file_name ".ml" then
	   filename__chop_suffix file_name ".ml", dotml_emacs_tags
	 else if filename__check_suffix file_name ".mli" then
	   filename__chop_suffix file_name ".mli", dotmli_emacs_tags
      	 else raise End_of_file (* beurk *) in
      hashtbl__add h mod_name (read_symbols ic (hashtbl__new 37))
   done
  with End_of_file -> ()
;;


let read_tags dir =
  if !use_tags then 
   let tagfile = filename__concat dir "TAGS" in
    try
     let ic = open_in tagfile in
     read_tag_file ic;
     close_in ic
    with
     sys__Sys_error _ -> 
       printf__fprintf stderr "Cannot open %s\n" tagfile;
       flush stderr
  | End_of_file ->
       printf__fprintf stderr "I say %s is empty\n" tagfile;
       flush stderr
;;

let clear_tags () =
  hashtbl__clear dotml_emacs_tags;
  hashtbl__clear dotmli_emacs_tags
;;
